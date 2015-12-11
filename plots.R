pdf("opg21.pdf", height=6, width=6)
plot(1, type="n", xlab="x", ylab="y", xlim=c(0, 2), ylim=c(0, 2))
polygon(c(0,0,1,1), c(0,3,3,1),col="grey")
dev.off()

pdf("opg22.pdf", height=6, width=6)
plot(1, type="n", xlab="x", ylab="y", xlim=c(0, 1), ylim=c(0, 1))
polygon(c(0,0,0.5), c(0,1,0.5),col="grey")
dev.off()
