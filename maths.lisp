(in-package :issac)

(defn calculate-spring-damper-acceleration ((dt single-float)
                                            (ks single-float)
                                            (x single-float)
                                            (kd single-float)
                                            (s single-float))
    single-float
  (the single-float (newtoncalculatespringdamperacceleration dt ks x kd s)))
