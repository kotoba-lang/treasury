(ns treasury.core
  "Domain-agnostic USDC payment quoting + on-chain verification + append-only
  ledger entries (ADR-2607051621). Extracted from gftdcojp/local-murakumo's
  itonami.cljc: pricing/units (credits-per-usd, plans, verticals, protocol-fee
  rate, …) stay in the consuming project and are injected as `fee-frac`; this
  namespace only knows USD, USDC, EVM chains, and a signed append-only run
  ledger shape.

  No custody: a payer sends USDC from their OWN wallet to a treasury address
  supplied by the caller (typically a Safe/multisig) — this library never
  holds keys or moves funds. A payment produces a ledger entry only AFTER
  on-chain confirmation; an unverified claim stays :pending so a fake or
  underpaid tx can't confirm anything."
  (:require [clojure.string :as str]))

;; ── chains ───────────────────────────────────────────────────────────────
;; USDC contract + block-explorer API per EVM chain. A Safe's address is the
;; same across chains (CREATE2), so switching `chain` moves the rail to a
;; cheaper L2 without changing the recipient.

(def chains
  {"ethereum" {:chain "ethereum" :usdc "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"
               :explorer-api "https://api.etherscan.io/api"
               :rpc "https://ethereum-rpc.publicnode.com"
               :fee-hint "gas 高め（$数）— 少額決済には L2 推奨"}
   "base"     {:chain "base" :usdc "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913"
               :explorer-api "https://api.basescan.org/api"
               ;; keyless JSON-RPC verify path (receipt->onchain). Preferred over
               ;; the explorer API — Basescan V1 is deprecated and Etherscan V2
               ;; requires a PAID plan for Base (chainid 8453). Base's public RPC
               ;; is keyless + free (ADR-2607093100 verify-path robustness).
               :rpc "https://mainnet.base.org"
               :fee-hint "gas 数セント — 少額決済向き"}
   "arbitrum" {:chain "arbitrum" :usdc "0xaf88d065e77c8cC2239327C5EDb3A432268e5831"
               :explorer-api "https://api.arbiscan.io/api"
               :rpc "https://arbitrum-one-rpc.publicnode.com"
               :fee-hint "gas 安い"}})

;; keccak256("Transfer(address,address,uint256)")
(def transfer-topic
  "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef")

(def default-chain "ethereum")
(defn chain-cfg [chain] (get chains (or chain default-chain) (get chains default-chain)))

(def crypto-asset {:asset "USDC" :decimals 6 :custody "safe-multisig"})
(def usdc-per-usd 1)
(def min-confirmations 3)

;; ── fee split (pure pricing primitive — no unit conversion) ────────────────

(defn fee-split
  "usd + fee-frac (e.g. 0.05 = 5%) -> {:usd :fee :net}. Pure. The caller
   decides what :net means downstream (credits minted, a registration marked
   paid, a protocol-fee credit accrued — this library doesn't know)."
  [usd fee-frac]
  (let [usd (double usd)
        fee (* usd (double fee-frac))]
    {:usd usd :fee fee :net (- usd fee)}))

;; ── quote / pending / confirmed entries ────────────────────────────────────

(defn crypto-quote
  "usd + fee-frac + treasury address (+ optional chain) -> the payment
   request as data: how much USDC to send, on which chain, to which treasury,
   and the fee split. Destination address is always caller-supplied, never
   hardcoded. Pure."
  ([usd fee-frac treasury] (crypto-quote usd fee-frac treasury default-chain))
  ([usd fee-frac treasury chain]
   (let [{:keys [usd fee net]} (fee-split usd fee-frac)
         {:keys [chain usdc fee-hint]} (chain-cfg chain)]
     (merge crypto-asset
            {:usd usd :chain chain :usdc-contract usdc :fee-hint fee-hint
             :treasury treasury
             :amount (* usd usdc-per-usd)
             :fee fee :net net}))))

(defn pending-entry
  "An UNVERIFIED crypto claim -> a pending ledger record (nothing confirmed
   yet). `kind` tags what this payment is FOR (:topup, :protocol-fee,
   :registration, …) — this library is agnostic to the meaning, it only
   round-trips the tag."
  ([kind did usd tx] (pending-entry kind did usd tx default-chain))
  ([kind did usd tx chain]
   {:run/kind :pending
    :run/for kind
    :treasury/pending-payer (name did)
    :treasury/payment {:asset (:asset crypto-asset) :chain (:chain (chain-cfg chain))
                        :usd usd :tx tx}
    :treasury/proof :crypto-pending}))

(defn confirmed-entry
  "A CONFIRMED crypto payment -> a ledger record. `net`/`fee` come from
   fee-split; the caller decides what :net triggers downstream."
  ([kind did usd fee-frac tx] (confirmed-entry kind did usd fee-frac tx default-chain))
  ([kind did usd fee-frac tx chain]
   (let [{:keys [fee net]} (fee-split usd fee-frac)]
     {:run/kind :confirmed
      :run/for kind
      :treasury/payer (name did)
      :treasury/net net
      :treasury/fee fee
      :treasury/payment {:asset (:asset crypto-asset) :chain (:chain (chain-cfg chain))
                          :usd usd :tx tx}
      :treasury/proof :crypto-confirmed})))

;; ── verification ────────────────────────────────────────────────────────

(defn- run-payment [run]
  (or (:treasury/payment run) (get run "treasury/payment") (:payment run) (get run "payment") {}))

(defn- run-proof [run]
  (or (:treasury/proof run) (get run "treasury/proof") (:proof run) (get run "proof")))

(defn- run-kind* [run]
  (or (:run/kind run) (get run "run/kind") (:kind run) (get run "kind")))

(defn- pending-payer [run]
  (or (:treasury/pending-payer run) (get run "treasury/pending-payer")))

(defn verify-payment
  "Decide whether a PENDING crypto claim is confirmed on-chain. Pure — the
   on-chain tx record is INJECTED (a real RPC/indexer fetches it; tests stub
   it), so the decision logic is unit-testable without a chain.
     pending : a pending-entry
     onchain : {:to <addr> :amount <usdc> :confirmations <n> :asset \"USDC\"}
               (nil = tx not found yet -> not confirmed)
     opts    : {:treasury <addr> :fee-frac <n> :min-confirmations <n>}
   -> {:ok? bool :reason kw :entry <confirmed-entry when ok>}. Wrong recipient
   / underpaid / too few confirmations all reject — a fake or insufficient tx
   cannot confirm."
  [pending onchain {:keys [treasury fee-frac min-confirmations]
                    :or {min-confirmations min-confirmations}}]
  (let [pay (run-payment pending)
        want-usd (or (:usd pay) (get pay "usd"))
        did (pending-payer pending)
        kind (or (:run/for pending) (get pending "run/for"))
        lc (fn [x] (some-> x str str/lower-case))
        to (or (:to onchain) (get onchain "to"))
        amount (or (:amount onchain) (get onchain "amount") 0)
        confs (or (:confirmations onchain) (get onchain "confirmations") 0)
        tx (or (:tx pay) (get pay "tx"))
        chain (or (:chain pay) (get pay "chain") default-chain)]
    (cond
      (nil? onchain)                {:ok? false :reason :tx-not-found}
      (not= (lc to) (lc treasury))  {:ok? false :reason :wrong-recipient}
      (< (double amount) (double (* want-usd usdc-per-usd)))
                                    {:ok? false :reason :underpaid}
      (< confs min-confirmations)  {:ok? false :reason :insufficient-confirmations}
      :else {:ok? true :reason :confirmed
             :entry (confirmed-entry kind did want-usd fee-frac tx chain)})))

(defn pending-payments
  "Ledger runs -> the crypto claims still awaiting confirmation: pending
   entries whose tx has NOT yet been confirmed. Pure and idempotent — a
   confirmed tx drops off, so a verifier loop never double-confirms."
  [runs]
  (let [confirmed-txs (into #{}
                             (comp (filter #(#{:crypto-confirmed "crypto-confirmed"} (run-proof %)))
                                   (keep (comp :tx run-payment)))
                             runs)]
    (for [r runs
          :when (#{:pending "pending"} (run-kind* r))
          :let [tx (:tx (run-payment r))
                did (pending-payer r)
                usd (or (:usd (run-payment r)) (get (run-payment r) "usd"))]
          :when (and tx did (not (confirmed-txs tx)))]
      {:did did :usd usd :tx tx})))

(defn payment-status
  "A payer's crypto payment status from the ledger -> {:pending [{:tx :usd}]
   :confirmed [{:tx :net}]}. Pure."
  [runs did]
  (let [mine? (fn [r] (= (name did)
                        (name (or (pending-payer r)
                                  (:treasury/payer r) (get r "treasury/payer") ""))))]
    {:pending (vec (for [r runs
                         :when (and (= "crypto-pending" (str (name (or (run-proof r) :none)))) (mine? r))]
                     {:tx (:tx (run-payment r)) :usd (:usd (run-payment r))}))
     :confirmed (vec (for [r runs
                           :when (and (= "crypto-confirmed" (str (name (or (run-proof r) :none)))) (mine? r))]
                       {:tx (:tx (run-payment r))
                        :net (or (:treasury/net r) (get r "treasury/net"))}))}))

(defn etherscan-row->onchain
  "Parse one Etherscan-family (Etherscan/Basescan/Arbiscan share the same
   `account/tokentx` shape) incoming ERC-20 transfer row into the on-chain
   record verify-payment expects. USDC has 6 decimals. Pure."
  [row]
  (let [g #(or (get row %) (get row (keyword %)))
        decimals (or (some-> (g "tokenDecimal") str parse-long) 6)
        raw (or (some-> (g "value") str parse-long) 0)]
    {:to (g "to")
     :amount (/ (double raw) (Math/pow 10 decimals))
     :confirmations (or (some-> (g "confirmations") str parse-long) 0)
     :asset (g "tokenSymbol")
     :tx (g "hash")}))

;; ── keyless JSON-RPC verify path (no explorer API) ─────────────────────────
;; The host fetches eth_getTransactionReceipt (the tx) and eth_blockNumber (the
;; head) from the chain's public :rpc and passes both here. Pure parsing of the
;; USDC Transfer log → the same on-chain record etherscan-row->onchain yields,
;; so verify-payment is unchanged. Survives Basescan V1 deprecation + avoids
;; Etherscan V2's paid-plan requirement for Base.

(defn hex->long
  "Parse a 0x-prefixed hex string to a long (nil on blank/garbage). USDC micros
   stay well under 2^53 so long is safe for amounts and block numbers. Public
   so callers parsing an eth_blockNumber RPC result (the `current-block` arg
   to receipt->onchain below) use this validating parser instead of a raw
   js/parseInt, which silently yields NaN on malformed/missing RPC output
   instead of nil."
  [h]
  (let [s (some-> h str str/lower-case (str/replace #"^0x" ""))]
    (when (and s (not= s "") (re-matches #"[0-9a-f]+" s))
      #?(:clj (Long/parseLong s 16)
         :cljs (js/parseInt s 16)))))

(defn- finite-number?
  "true for a real, non-NaN number. NaN is truthy in Clojure/ClojureScript's
   `if`/`and`/`when` (only nil/false are falsy) and NaN comparisons
   (`<`/`>`/`>=`/`<=`) are always false -- an un-validated NaN silently
   passes any `(and ... x)` truthiness gate yet defeats any numeric bound
   check downstream. receipt->onchain guards `current-block` with this
   because it is EXTERNAL I/O-derived (a caller-supplied eth_blockNumber
   result) and a caller might not use hex->long to parse it.

   NOT `(= x x)` -- that idiom reliably catches NaN on :cljs, but on :clj a
   boxed Double NaN passed through a function boundary compares `.equals`
   (which Java specifically defines as NaN.equals(NaN) => true), not IEEE754
   `==` (NaN == NaN => false) -- `(= x x)` silently returns true for NaN
   there instead of false, confirmed empirically against a real JVM (a
   top-level `(let [x Double/NaN] (= x x))` returns false, but the exact
   same value routed through a fn argument returns true). Double/isNaN
   sidesteps this entirely."
  [x]
  (and (number? x) #?(:clj (not (Double/isNaN (double x)))
                       :cljs (not (js/isNaN x)))))

(defn- topic->address
  "A 32-byte log topic (0x + 24 zero-bytes + 20-byte address) → 0x<40hex> addr."
  [topic]
  (let [s (some-> topic str str/lower-case (str/replace #"^0x" ""))]
    (when (and s (>= (count s) 40))
      (str "0x" (subs s (- (count s) 40))))))

(defn receipt->onchain
  "Parse an eth_getTransactionReceipt result + the current head block number
   into the on-chain record verify-payment expects, for the USDC Transfer to
   the treasury. Pure — the RPC I/O is the host's. Returns nil when the tx
   reverted or carries no matching USDC Transfer log.
     receipt      : {\"status\" \"0x1\" \"blockNumber\" \"0x…\"
                     \"transactionHash\" \"0x…\"
                     \"logs\" [{\"address\" \"0x…\" \"topics\" [t0 from to] \"data\" \"0x…\"}]}
     current-block: integer head block (from eth_blockNumber)
     usdc-contract: the chain's USDC address"
  [receipt current-block usdc-contract]
  (let [g #(or (get receipt %) (get receipt (keyword %)))
        status (g "status")
        logs (or (g "logs") [])
        usdc (str/lower-case (str usdc-contract))
        tx-block (hex->long (g "blockNumber"))
        transfer (some (fn [lg]
                         (let [la #(or (get lg %) (get lg (keyword %)))
                               topics (or (la "topics") [])]
                           (when (and (= (str/lower-case (str (la "address"))) usdc)
                                      (= (str/lower-case (str (first topics))) transfer-topic)
                                      (>= (count topics) 3))
                             lg)))
                       logs)]
    (when (and transfer
               ;; success only ("0x1"); "0x0" = reverted → nil (cannot confirm)
               (contains? #{"0x1" 1 "0x01"} status)
               tx-block
               ;; NOT just `current-block` -- a NaN current-block (e.g. a
               ;; caller that parsed a failed/malformed eth_blockNumber RPC
               ;; result with a raw js/parseInt instead of hex->long) is
               ;; truthy here and would otherwise flow into :confirmations
               ;; below as NaN, silently defeating verify-payment's
               ;; `(< confs min-confirmations)` guard (NaN < n is always
               ;; false), so an unconfirmed payment could be accepted as
               ;; confirmed.
               (finite-number? current-block))
      (let [la #(or (get transfer %) (get transfer (keyword %)))
            topics (la "topics")
            raw (or (hex->long (la "data")) 0)]
        {:to (topic->address (nth topics 2))
         :amount (/ (double raw) (Math/pow 10 6))     ; USDC 6 decimals
         :confirmations (max 0 (inc (- current-block tx-block)))
         :asset "USDC"
         :tx (g "transactionHash")}))))
