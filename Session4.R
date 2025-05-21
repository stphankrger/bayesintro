#Data Generation

dataGeneration <- function(n, p) {
  rbinom(n, 1, p)
}

countfct <- function(d, cp) {
  
  sides <- length(cp)-1
  Land <- sum(d == 1)
  Water <- sum(d == 0)
  table <- cbind(Land, Water)
  ways <- (cp*sides)^Land * ((1-cp)*sides)^Water
  data.frame(cp, ways)
  
}


countfctPercent <- function(d, cp) {
  sides <- length(cp)-1
  Land <- sum(d == 1)
  Water <- sum(d == 0)
  ways <- (cp*sides)^Land * ((1-cp)*sides)^Water
  waysTotal <- sum(ways)
  waysPercent <- ways/waysTotal
  data.frame(cp, ways, rep(waysTotal, length(cp)), waysPercent)
  
}

countfct(dataGeneration(100, 0.4), seq(0,1,.25))


#----

oldData <- countfct(dataGeneration(100, 0.4), seq(0,1,.25))
newData <- countfct(1, seq(0,1,.25))

newData

viewData <- data.frame(cp = oldData$cp, waysOld = oldData$ways, 
                       waysNew = newData$ways, total = oldData$ways * newData$ways)

countfctPercent(dataGeneration(100, 0.4), seq(0,1,.25))







