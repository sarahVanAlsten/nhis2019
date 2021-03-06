#function to easily print out all coefs nicely

printCoef <- function(mod){
  ti <- broom::tidy(mod, exponentiate = T, conf.int =T)
    writeLines(noquote(paste0(sprintf(round(ti$estimate, 3),fmt = "%.3f"), " (",
                   sprintf(round(ti$conf.low, 3),fmt= "%.3f"), " - ",
                   sprintf(round(ti$conf.high, 3),fmt= "%.3f"), ")")))

  
}

#print model coefficients

#dz specific
printCoef(mod2.diab.sa)
printCoef(mod2.diab.sa.l)
printCoef(mod2.early.diab)
printCoef(mod2.cvd.sa)
printCoef(mod2.cvd.sa.l)
printCoef(mod2.early.cvd)
printCoef(mod2.cvdht.sa)
printCoef(mod2.cvdht.sa.l)
printCoef(mod2.early.cvdht)

#all cause
printCoef(mod2.diab.allcause)
printCoef(mod2.early.diab.allcause)
printCoef(mod2.diab.sa.l.ac)
printCoef(mod2.cvd.allcause)
printCoef(mod2.early.cvd.allcause)
printCoef(mod2.cvd.sa.l.ac)
printCoef(mod2.cvdht.allcause)
printCoef(mod2.early.cvdht.allcause)
printCoef(mod2.cvdht.sa.l.ac)


#hypertension
printCoef(mod2.ht.sa)
printCoef(mod2.ht.sa.l)
printCoef(mod2.early.ht)

printCoef(mod2.ht.allcause)
printCoef(mod2.ht.sa.l.ac)
printCoef(mod2.early.ht.allcause)
