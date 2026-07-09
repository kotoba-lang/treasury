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
    (testing "confirms on correct recipient/amount/confirmations (case-insensitive address match)"
      (let [result (t/verify-payment pending {:to "0xsafe" :amount 10 :confirmations 3} opts)]
        (is (:ok? result))
        (is (= :confirmed (:reason result)))
        (is (= :topup (:run/for (:entry result))))
        (is (= 9.5 (:treasury/net (:entry result))))))))

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
                                    {:treasury treasury :fee-frac 0.0 :min-confirmations 3})))))))
