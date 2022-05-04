(ns hospital.logic-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [hospital.logic :refer :all]
            [hospital.model :as h.model]
            [clojure.test.check.clojure-test :refer (defspec)]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [schema.core :as s])
  (:import (clojure.lang ExceptionInfo)))

(s/set-fn-validation! true)

; são testes ESCRITOS baseados em exemplos
(deftest cabe-na-fila?-test

  (testing "Que cabe numa vila fazia"
    (is (cabe-na-fila? {:espera []} :espera)))

  ; o doseq com um simbolo e uma sequencia gerada funciona
  ; mas.... talvez não seja o que queremos em example-based manual, vamos
  ; ver um problema de dois simbolos
  (testing "Que cabe pessoas em filas de tamanhao ate 4 inclusive"
    (doseq [fila (gen/sample (gen/vector gen/string-alphanumeric 0 4) 100)]
      (is (cabe-na-fila? {:espera fila}, :espera))))


    (testing "Que não cabe na fila quando a fila está cheia"
      (is (not (cabe-na-fila? {:espera [1 5 37 54 21]}, :espera))))

  ; one off da borda do limite pra cima
  (testing "Que não cabe na fila quando tem mais do que uma fila cheia"
    (is (not (cabe-na-fila? {:espera [1 2 3 4 5 6]}, :espera))))

  ; dentro das bordas
  (testing "Que cabe na fila quando tem gente mas não está cheia"
    (is (cabe-na-fila? {:espera [1 2 3 4]}, :espera))
    (is (cabe-na-fila? {:espera [1 2]}, :espera)))

  (testing "Que não cabe quando o departamento não existe"
    (is (not (cabe-na-fila? {:espera [1 2 3 4]}, :raio-x)))))

; aqui tivemos um problema
; o doseq na unha gera uma multiplicacao de casos
; incluindo muuuuuuuitos casos repetidos
; que nao tem nada ligado com o que queremos
;(deftest chega-em-test
;  (testing "Que é colocada uma pessoa em filas menores que 5"
;    (doseq [fila (gen/sample (gen/vector gen/string-alphanumeric 0 4) 10)
;            pessoa (gen/sample gen/string-alphanumeric 5)]
;      (println pessoa fila)
;      (is (= 1 1)) ; só para mostrarq que são 50 asserts (10 * 5)
;      )
;    ))

; muito importante lembrar que se voce esta rodando um repl continuo e recarregando
; os testes. você corre o risco de uma função que foi definida antigamente continuar
; carregada no seu namespace e continuar rodando ela. nesse caso lembrar de restart o repl

; o teste a seguir é generativo e funciona
; mas... o resultado dele parece MUITO uma cópia do nosso código implementado
; {:espera (conj fila pessoa)} == o codigo que eu escrevi la
; se eu coloquei um bug lá, provavelmente eu coloquei um bug aqui, e ai vai dar true e o bug continua.
; nao importa se escreve o teste ANTES ou DEPOS... é o mesmo codigo. (não é 100%)
; mas a medida que ficasse mais complexo o teste.
(defspec coloca-uma-pessoa-em-filas-menores-que-5 100
         (prop/for-all
           [fila (gen/vector gen/string-alphanumeric 0 4)
            pessoa gen/string-alphanumeric]
           (is (= {:espera (conj fila pessoa)}
                  (chega-em {:espera fila} :espera pessoa)))))

; coloquei sufixo, mas voce vai ver prefixo também no mundo
; não sou maior fã, mas é o que a vida nos oferece
(def nome-aleatorio
  (gen/fmap clojure.string/join
            (gen/vector gen/char-alphanumeric 5 10)))

(defn transforma-vetor-em-fila [vetor]
  (reduce conj h.model/fila-vazia vetor))

(def fila-nao-cheia-gen
  (gen/fmap
    transforma-vetor-em-fila
    (gen/vector nome-aleatorio 0 4)))

(defn transfere-ignorando-erro [hospital para]
  (try
    (transfere hospital :espera para)
    (catch clojure.lang.ExceptionInfo e
      ;(println "falhou" e)
      hospital
      )))

(defspec transfere-tem-que-manter-a-quantidade-de-pessoas 50
         (prop/for-all
           [espera (gen/fmap transforma-vetor-em-fila (gen/vector nome-aleatorio 0 50))
            raio-x fila-nao-cheia-gen
            ultrasom fila-nao-cheia-gen
            vai-para (gen/vector (gen/elements [:raio-x :ultrasom]) 0 50)
            ]
           ;(println (count espera)(count vai-para) vai-para)
           (let [hospital-inicial {:espera espera, :raio-x raio-x, :ultrasom ultrasom}
                 hospital-final (reduce transfere-ignorando-erro hospital-inicial  vai-para)]
             ;(println (count (get hospital-final :raio-x)))
             (= (total-de-pacientes hospital-inicial)
                (total-de-pacientes hospital-final))
             )
           ))
























