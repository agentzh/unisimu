(defmodule EVAL
    (import VECTORIZE deftemplate vector-relation))

(defmodule ANTIVECTORIZE
    (import VECTORIZE deftemplate vector-relation)
    (import VECTORIZE deftemplate space-relation))
