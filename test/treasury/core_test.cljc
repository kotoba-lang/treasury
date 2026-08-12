(ns treasury.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [treasury.core :as t]))

(deftest fee-split-test
  (testing "5% fee split"
    (is (= {:usd 10.0 :fee 0.5 :net 9.5} (t/fee-split 10 0.05))))
  (testing "0% fee split is a no-op"
    (is (= {:usd 20.0 :fee 0.0 :net 20.0} (t/fee-split 20 0)))))

(deftest crypto-quote-test
  (testing "default chain is ethereum"
    (let [q (t/crypto-quote 10 0.05 "0xTREASURY")]
      (is (= "ethereum" (:chain q)))
      (is (= "0xTREASURY" (:treasury q)))
      (is (= 10.0 (:amount q)))
      (is (= 0.5 (:fee q)))
      (is (= 9.5 (:net q)))))
  (testing "chain override moves the rail, not the treasury"
    (let [q (t/crypto-quote 10 0.05 "0xTREASURY" "base")]
      (is (= "base" (:chain q)))
      (is (= "0xTREASURY" (:treasury q))))))

(deftest pending-and-confirmed-entry-test
  (testing "pending-entry carries the :run/for tag opaquely"
    (let [p (t/pending-entry :protocol-fee "did:key:zAlice" 10 "0xTX1")]
      (is (= :pending (:run/kind p)))
      (is (= :protocol-fee (:run/for p)))
      (is (= "did:key:zAlice" (:treasury/pending-payer p)))
      (is (= :crypto-pending (:treasury/proof p)))))
  (testing "confirmed-entry applies the fee split"
    (let [c (t/confirmed-entry :registration "did:key:zBob" 100 0.05 "0xTX2")]
      (is (= :confirmed (:run/kind c)))
      (is (= :registration (:run/for c)))
      (is (= 5.0 (:treasury/fee c)))
      (is (= 95.0 (:treasury/net c))))))

(deftest verify-payment-test
  (let [pending (t/pending-entry :topup "did:key:zAlice" 10 "0xTX")
        opts {:treasury "0xSAFE" :fee-frac 0.05 :min-confirmations 3}]
    (testing "rejects when tx not found"
      (is (= :tx-not-found (:reason (t/verify-payment pending nil opts)))))
    (testing "rejects wrong recipient (case-insensitive match, but must match)"
      (is (= :wrong-recipient
             (:reason (t/verify-payment pending {:to "0xNOTSAFE" :amount 10 :confirmations 5} opts)))))
    (testing "rejects underpaid"
      (is (= :underpaid
             (:reason (t/verify-payment pending {:to "0xSAFE" :amount 5 :confirmations 5} opts)))))
    (testing "rejects insufficient confirmations"
      (is (= :insufficient-confirmations
             (:reason (t/verify-payment pending {:to "0xSAFE" :amount 10 :confirmations 1} opts)))))
    (testing "confirms on correct recipient/amount/confirmations/asset (case-insensitive address+asset match)"
      (let [result (t/verify-payment pending {:to "0xsafe" :amount 10 :confirmations 3 :asset "usdc"} opts)]
        (is (:ok? result))
        (is (= :confirmed (:reason result)))
        (is (= :topup (:run/for (:entry result))))
        (is (= 9.5 (:treasury/net (:entry result))))))
    (testing "CONFIRMED BUG regression: rejects a wrong asset even when recipient/amount/
              confirmations are all correct -- a payment in a worthless/fake token (e.g.
              etherscan-row->onchain's unfiltered tokenSymbol) must never be confirmed as
              a genuine USDC payment just because the amount/recipient numerically match"
      (is (= :wrong-asset
             (:reason (t/verify-payment pending
                                        {:to "0xSAFE" :amount 10 :confirmations 5 :asset "SCAMCOIN"}
                                        opts)))))
    (testing "rejects a missing asset too -- an onchain record that doesn't self-report a
              token type must fail closed, not be trusted by default"
      (is (= :wrong-asset
             (:reason (t/verify-payment pending
                                        {:to "0xSAFE" :amount 10 :confirmations 5}
                                        opts)))))))

(deftest pending-payments-test
  (testing "a pending tx with no matching confirmed entry still shows as pending"
    (let [runs [(t/pending-entry :topup "did:key:zAlice" 10 "0xTX1")]]
      (is (= [{:did "did:key:zAlice" :usd 10 :tx "0xTX1"}] (t/pending-payments runs)))))
  (testing "a confirmed tx drops off pending-payments"
    (let [runs [(t/pending-entry :topup "did:key:zAlice" 10 "0xTX1")
                (t/confirmed-entry :topup "did:key:zAlice" 10 0.05 "0xTX1")]]
      (is (= [] (t/pending-payments runs))))))

(deftest payment-status-test
  (let [runs [(t/pending-entry :topup "did:key:zAlice" 10 "0xTX1")
              (t/confirmed-entry :topup "did:key:zAlice" 20 0.05 "0xTX2")
              (t/confirmed-entry :topup "did:key:zBob" 5 0.05 "0xTX3")]]
    (testing "only returns the requested did's entries"
      (let [status (t/payment-status runs "did:key:zAlice")]
        (is (= [{:tx "0xTX1" :usd 10}] (:pending status)))
        (is (= [{:tx "0xTX2" :net 19.0}] (:confirmed status)))))))

(deftest etherscan-row->onchain-test
  (testing "parses a raw Etherscan-family tokentx row (string keys, 6-decimal USDC)"
    (is (= {:to "0xSAFE" :amount 10.0 :confirmations 12 :asset "USDC" :tx "0xTX"}
           (t/etherscan-row->onchain
            {"to" "0xSAFE" "value" "10000000" "tokenDecimal" "6"
             "confirmations" "12" "tokenSymbol" "USDC" "hash" "0xTX"})))))

(def usdc "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913")
(def transfer-topic "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef")

(defn- usdc-log [to data-hex]
  {"address" usdc
   "topics" [transfer-topic
             "0x000000000000000000000000aaaa000000000000000000000000000000000001" ; from
             (str "0x000000000000000000000000" (subs to 2))]                      ; to (padded)
   "data" data-hex})

(deftest receipt->onchain-test
  (let [treasury "0xbbbb000000000000000000000000000000000002"
        receipt {"status" "0x1" "blockNumber" "0x64" "transactionHash" "0xTX"
                 "logs" [(usdc-log treasury "0x7a120")]}]                         ; 500000 = 0.5 USDC
    (testing "parses a successful USDC Transfer receipt via RPC (keyless path)"
      (is (= {:to treasury :amount 0.5 :confirmations 4 :asset "USDC" :tx "0xTX"}
             (t/receipt->onchain receipt 103 usdc))))                            ; head 103, block 100 → 4 conf
    (testing "reverted tx (status 0x0) → nil, cannot confirm"
      (is (nil? (t/receipt->onchain (assoc receipt "status" "0x0") 103 usdc))))
    (testing "no USDC Transfer log → nil"
      (is (nil? (t/receipt->onchain {"status" "0x1" "blockNumber" "0x64" "logs" []} 103 usdc))))
    (testing "a non-USDC contract log is ignored → nil"
      (is (nil? (t/receipt->onchain
                 {"status" "0x1" "blockNumber" "0x64"
                  "logs" [(assoc (usdc-log treasury "0x7a120") "address" "0xdeadbeef")]}
                 103 usdc))))
    (testing "the parsed record flows through verify-payment unchanged"
      (let [pending (t/pending-entry :x402 "0xagent" 0.5 "0xTX" "base")
            onchain (t/receipt->onchain receipt 200 usdc)]
        (is (:ok? (t/verify-payment pending onchain
                                    {:treasury treasury :fee-frac 0.0 :min-confirmations 3})))))
    (testing "a NaN current-block (e.g. from a caller's raw js/parseInt on a
              failed/malformed eth_blockNumber RPC result) is rejected as
              nil instead of silently flowing into :confirmations as NaN --
              NaN is truthy in `and`/`when` and NaN < n is always false, so
              an un-guarded NaN would otherwise defeat verify-payment's
              min-confirmations check and accept an unconfirmed payment"
      #?(:cljs
         (is (nil? (t/receipt->onchain receipt js/NaN usdc)))
         :clj
         (is (nil? (t/receipt->onchain receipt Double/NaN usdc)))))))

;; ══ a contract recipient exists only where it was deployed (2026-07-26) ══
;; The old chains comment advised moving the rail to a cheaper L2 without changing
;; the recipient, on the grounds that a Safe's address is the same across chains.
;; A real Safe disproved the safety of that: deployed on Ethereum, NO CODE on BSC,
;; Avalanche, Base, Polygon, Arbitrum or Optimism.

(deftest contract-deployed?-reads-eth-getCode
  (is (true? (t/contract-deployed? "0x6080604052")))
  (is (false? (t/contract-deployed? "0x")) "empty = nothing on THIS chain")
  (is (false? (t/contract-deployed? nil))))

(deftest code-request-is-data-only
  (is (= {:jsonrpc "2.0" :id 1 :method "eth_getCode"
          :params ["0x640404B566D34c401996eBb360F40BC4cECFA881" "latest"]}
         (t/code-request "0x640404B566D34c401996eBb360F40BC4cECFA881"))))

(deftest safe-recipient-on-a-chain-without-code-is-refused
  (let [safe {:address "0x640404B566D34c401996eBb360F40BC4cECFA881" :chain "base"}
        {:keys [ok? problem]} (t/verify-recipient-deployed safe "0x")]
    (is (false? ok?))
    (is (= :recipient-has-no-code problem)
        "USDC sent here would be unrecoverable by anyone")))

(deftest safe-recipient-on-its-own-chain-passes
  (let [safe {:address "0x640404B566D34c401996eBb360F40BC4cECFA881" :chain "ethereum"}]
    (is (:ok? (t/verify-recipient-deployed safe "0x608060405273")))))

(deftest an-eoa-recipient-must-be-declared-as-one
  (testing "an EOA legitimately has no code, but the caller has to say so"
    (is (:ok? (t/verify-recipient-deployed
               {:address "0xabc" :chain "base" :expect-contract? false} "0x")))
    (is (= :recipient-unexpectedly-a-contract
           (:problem (t/verify-recipient-deployed
                      {:address "0xabc" :chain "base" :expect-contract? false}
                      "0x6080"))))))

;; ── materialized view (ADR-2608010000) ──────────────────────────────

(def ^:private usdc-base "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913")
(def ^:private TREASURY "0xA00366234D29d4F882088048c0B2fa0dB7302D4E")
(def ^:private xfer "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef")

(defn- topic-of [addr] (str "0x000000000000000000000000" (subs addr 2)))

(defn- hex [n]
  ;; portable: Long/toHexString is static on the JVM, .toString takes a radix in JS
  #?(:clj (Long/toHexString (long n)) :cljs (.toString n 16)))

(defn- log-of [{:keys [contract from to micros block tx topic]}]
  {"address" (or contract usdc-base)
   "topics" [(or topic xfer) (topic-of from) (topic-of to)]
   "data" (str "0x" (hex micros))
   "blockNumber" (str "0x" (hex block))
   "transactionHash" tx})

(def ^:private real-log
  (log-of {:from "0xe255D68563C974ac061484cEce4E57de02a4E0Da" :to TREASURY
           :micros 100000 :block 49351743 :tx "0x50c58ac4"}))

(def ^:private view-opts {:chain "base" :watched #{TREASURY} :from-block 49351000 :to-block 49352000})

(deftest view-admits-only-real-usdc-transfers-to-watched-addresses
  (let [logs [real-log
              ;; a worthless token whose symbol is "USDC" — wrong contract
              (log-of {:contract "0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
                       :from "0xe255D68563C974ac061484cEce4E57de02a4E0Da" :to TREASURY
                       :micros 999999999 :block 49351744 :tx "0xfake"})
              ;; right contract, but not a Transfer
              (log-of {:topic "0x1111111111111111111111111111111111111111111111111111111111111111"
                       :from "0xe255D68563C974ac061484cEce4E57de02a4E0Da" :to TREASURY
                       :micros 500000 :block 49351745 :tx "0xnottransfer"})
              ;; real transfer, but to someone we do not watch
              (log-of {:from "0xe255D68563C974ac061484cEce4E57de02a4E0Da"
                       :to "0x1111111111111111111111111111111111111111"
                       :micros 700000 :block 49351746 :tx "0xnotours"})]
        v (t/logs->view logs view-opts)]
    (is (= 1 (count (:entries v))) "only the genuine watched USDC Transfer survives")
    (is (= "0x50c58ac4" (:tx (first (:entries v)))))
    (is (= 100000 (:micros (first (:entries v)))))))

(deftest view-digest-is-canonical-not-arrival-ordered
  (testing "the same facts arriving in any order digest identically — this is
            what makes the digest usable as a memo key"
    (let [a (log-of {:from "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :to TREASURY
                     :micros 10000 :block 49351100 :tx "0xaa"})
          b (log-of {:from "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :to TREASURY
                     :micros 20000 :block 49351200 :tx "0xbb"})]
      (is (= (t/view-digest (t/logs->view [a b] view-opts))
             (t/view-digest (t/logs->view [b a] view-opts))))))
  (testing "and a different range is a different key even with the same entries"
    (is (not= (t/view-digest (t/logs->view [real-log] view-opts))
              (t/view-digest (t/logs->view [real-log] (assoc view-opts :to-block 49353000)))))))

(deftest absent-from-the-view-is-never-reported-as-absent-from-the-chain
  (testing "THE invariant: :not-in-view, never :not-found. A view is incomplete
            by construction, so its silence is not evidence about the chain —
            and telling a payer their transaction does not exist is the exact
            production defect this design removes"
    (let [v (t/logs->view [real-log] view-opts)
          r (t/verify-from-view v {:tx "0xsomethingelse" :treasury TREASURY :usd 0.01
                                   :head-block 49351800})]
      (is (false? (:ok? r)))
      (is (= :not-in-view (:reason r)))
      (is (not= :tx-not-found (:reason r)))))
  (testing "view-covers? is the only thing that can qualify that silence, and it
            needs a block — which is precisely what a miss does not give us"
    (let [v (t/logs->view [real-log] view-opts)]
      (is (true? (t/view-covers? v 49351743)))
      (is (false? (t/view-covers? v 49999999)) "outside the range")
      (is (false? (t/view-covers? v nil)) "unknown position is not covered"))))

(deftest verify-from-view-matches-the-live-path-on-the-real-payment
  (let [v (t/logs->view [real-log] view-opts)
        r (t/verify-from-view v {:tx "0x50c58ac4" :treasury TREASURY :usd 0.01
                                 :head-block 49351800 :min-confirmations 3})]
    (is (true? (:ok? r)))
    (is (= :confirmed (:reason r)))
    (is (= 0.1 (:amount (:onchain r))) "100000 micros = 0.1 USDC")
    (is (= "USDC" (:asset (:onchain r))))))

(deftest conclusive-failures-are-distinguished-from-inconclusive-ones
  (let [v (t/logs->view [real-log] view-opts)
        base {:tx "0x50c58ac4" :treasury TREASURY :usd 0.01 :head-block 49351800}]
    (testing "underpaid is CONCLUSIVE — the view has the entry and it is short"
      (is (= :underpaid (:reason (t/verify-from-view v (assoc base :usd 10.0))))))
    (testing "wrong recipient is conclusive"
      (is (= :wrong-recipient
             (:reason (t/verify-from-view v (assoc base :treasury "0x9999999999999999999999999999999999999999"))))))
    (testing "too few confirmations is conclusive-for-now"
      (is (= :insufficient-confirmations
             (:reason (t/verify-from-view v (assoc base :head-block 49351743 :min-confirmations 10))))))
    (testing "none of these are :not-in-view — the caller must not re-ask a node"
      (doseq [o [(assoc base :usd 10.0)
                 (assoc base :treasury "0x9999999999999999999999999999999999999999")]]
        (is (not= :not-in-view (:reason (t/verify-from-view v o))))))))

;; ── RPC fallback ordering ────────────────────────────────────────────────
;; Ported from network-awai/nexus-x402, where this lived as a LOCAL edit to a
;; vendored copy of this file (com-junkawasaki/root ADR-2608130700). A local
;; edit to a vendored copy is invisible to everyone else running the same
;; verify path, and it makes the copy unverifiable against any upstream commit.

(deftest chain-rpcs-base-has-fallbacks
  (testing "primary rpc first, then distinct fallbacks"
    (let [rpcs (t/chain-rpcs "base")]
      (is (= "https://mainnet.base.org" (first rpcs)))
      (is (some #{"https://1rpc.io/base"} rpcs))
      (is (some #{"https://base.meowrpc.com"} rpcs))
      (is (= (count rpcs) (count (distinct rpcs)))))))

(deftest chain-rpcs-degrades-to-a-single-endpoint
  (testing "a chain with no :rpcs still yields its primary, not an empty list"
    (is (= ["https://arbitrum-one-rpc.publicnode.com"] (t/chain-rpcs "arbitrum"))))
  (testing "an unknown chain falls back to the default chain's endpoints"
    (is (= (t/chain-rpcs t/default-chain) (t/chain-rpcs "no-such-chain")))))
