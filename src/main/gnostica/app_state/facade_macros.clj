(ns gnostica.app-state.facade-macros)

(defn- export-symbols [export-var]
  (when-not (qualified-symbol? export-var)
    (throw (ex-info "Facade export table must be a qualified symbol."
                    {:export-var export-var})))
  (let [resolved-var (requiring-resolve export-var)
        exports (var-get resolved-var)]
    (when-not (sequential? exports)
      (throw (ex-info "Facade export table must resolve to a sequence."
                      {:export-var export-var
                       :resolved-value exports})))
    (doseq [export exports]
      (when-not (simple-symbol? export)
        (throw (ex-info "Facade exports must be simple symbols."
                        {:export-var export-var
                         :export export}))))
    exports))

(defmacro def-facade-aliases [source-alias export-var]
  (when-not (simple-symbol? source-alias)
    (throw (ex-info "Facade source alias must be a simple symbol."
                    {:source-alias source-alias})))
  `(do
     ~@(for [export (export-symbols export-var)]
         `(def ~export ~(symbol (name source-alias)
                                (name export))))))
