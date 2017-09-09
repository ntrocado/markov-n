(defsystem "markov-n"
  :description "Markov chains on sample level with pcm audio files."
  :version "1"
  :author "Nuno Trocado"
  :depends-on ("alexandria")
  :components ((:file "packages")
               (:file "alias-method" :depends-on ("packages"))
               (:file "markov-n" :depends-on ("alias-method"))))
