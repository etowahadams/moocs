;; P4

(define (ouster t br)
  (cond [(false? br) false]
        [else
         (local
           (if (string=? t (bracket-team-lost br))
               (bracket-team-won br)
               (local [define ouster-won (ouster t (bracket-br-won br))]
                 (if (not (false? ouster-won)))
                    ouster-won
                    (ouster t (bracket-br-lost br)))))]))