data <- data.frame(
                         prize_door = c("A","A","A","A","B","B","B","B","C","C","C","C"),
                         guest_door = c("A","A","B","C","B","B","A","C","C","C","A","B"),
                         monty_door = c("B","C","C","B","A","C","C","A","B","A","B","A")
                         )

library(bnlearn)

other <- empty.graph(nodes = c("prize_door","guest_door","monty_door"))
other <- set.arc(other,"prize_door","monty_door")
other <- set.arc(other,"guest_door","monty_door")

fit <- bn.fit(other, data = data)

library(gRain)

jtree <- compile(as.grain(fit))

evnodes = c("guest_door","monty_door")

ev <- setFinding(jtree, nodes = evnodes, states = c("A","B"))
qr <- querygrain(ev, nodes = "prize_door", type = "marginal")
print(qr)

ev <- setFinding(jtree, nodes = evnodes, states = c("C","A"))
qr <- querygrain(ev, nodes = "prize_door", type = "marginal")
print(qr)
