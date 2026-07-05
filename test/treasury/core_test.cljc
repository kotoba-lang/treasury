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
