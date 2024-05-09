(ns lemmalearnerclj.textdatastructures)


(defrecord Text [title paragraphs])
(defrecord Paragraph [raw sentences])
(defrecord Sentence [raw sub-paragraphs words])
(defrecord Conjugation [raw])
(defrecord Lemma [raw])
