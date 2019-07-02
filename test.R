rand <- function() {
    i <- floor(runif(1,0,2))
    r <- "on"
    if (i == 0) {
        r = "off"
    }
    return(r)
}

randvector <- function() {
    r <- rand()
    z <- rand()
    p <- rand()
    e <- "off"
    if (r == "on" || z == "on") {
        if (p == "off") {
            e = "on"
        }
    }
    return(c(r,z,rand(),rand(),rand(),p,e,
             rand(),rand(),rand(),rand(),rand(),rand(),rand(),rand()))
}

randNodes <- function(n) {
    cnt <- 0
    result <- data.frame()
    while (cnt < n) {
        r <- rand()
        z <- rand()
        p <- rand()
        e <- "off"
        if (r == "on" || z == "on") {
            if (p == "off") {
                e = "on"
            }
        }
        order = c("on","off")
        tmp <- data.frame(
                                retranslost = factor(r, order),
                                zwindelay = factor(z, order),
                                delayedack = factor(rand(), order),
                                connecterror = factor(rand(), order),
                                lltcexend =factor(rand(), order),
                                parent_cause = factor(p, order),
                                error = factor(e, order),
                                    noise1 = factor(rand(), order),
                                    noise2 = factor(rand(), order),
                                    noise3 = factor(rand(), order),
                                    noise4 = factor(rand(), order),
                                    noise5 = factor(rand(), order),
                                    noise6 = factor(rand(), order),
                                    noise7 = factor(rand(), order),
                                    noise8 = factor(rand(), order)
        )
        result <- rbind(result, tmp)
        cnt = cnt + 1
    }
    return(result)
}

alert.data <- randNodes(500)

str(alert.data)

library(bnlearn)

alert.hc <- hc(alert.data, score = "aic")

alert.other <- empty.graph(nodes = nodes(alert.hc))
alert.other <- set.arc(alert.other,"error","retranslost")
alert.other <- set.arc(alert.other,"error","lltcexend")
alert.other <- set.arc(alert.other,"error","connecterror")
alert.other <- set.arc(alert.other,"error","zwindelay")
alert.other <- set.arc(alert.other,"error","delayedack")
alert.other <- set.arc(alert.other,"parent_cause","retranslost")
alert.other <- set.arc(alert.other,"parent_cause","lltcexend")
alert.other <- set.arc(alert.other,"parent_cause","connecterror")
alert.other <- set.arc(alert.other,"parent_cause","zwindelay")
alert.other <- set.arc(alert.other,"parent_cause","delayedack")

alert.other <- set.arc(alert.other,"noise1","retranslost")
alert.other <- set.arc(alert.other,"noise2","lltcexend")
alert.other <- set.arc(alert.other,"noise3","connecterror")
alert.other <- set.arc(alert.other,"noise4","zwindelay")
alert.other <- set.arc(alert.other,"noise5","zwindelay")
alert.other <- set.arc(alert.other,"noise6","lltcexend")
alert.other <- set.arc(alert.other,"noise7","delayedack")
alert.other <- set.arc(alert.other,"noise8","retranslost")

alert.fit <- bn.fit(alert.other, data = alert.data)

#alert.m <- moral(alert.other)

library(gRain)

alert.jtree <- compile(as.grain(alert.fit))

evnodes=c("retranslost","zwindelay","delayedack","connecterror","lltcexend","parent_cause",
            "noise1","noise2","noise3","noise4","noise5","noise6","noise7","noise8")

testloop = 200
sam = 0
i = 1
while (i <= testloop) {
    v = randvector()
    st = v[c(1,2,3,4,5,6,8,9,10,11,12,13,14,15)]
    flag = v[7]
    ev<-setFinding(alert.jtree, nodes=evnodes,states=st)
    qr<-querygrain(ev,nodes="error",type="marginal")$error[1]
    guess = "off"
    if (qr > 0.9) {
        guess = "on"
    }
    if (guess == flag) {
        sam = sam + 1
    }
    i = i + 1
    #print(v)
    #print(guess)
}

print("MODEL ACCURACY")
print(sam/testloop)
