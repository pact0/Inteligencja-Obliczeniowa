#załóżmy że mamy dwie funkcje celu
#jedna ma 3 parametry a druga 2
a1<-2
b1<-1
c1<-2
b2<-3
c2<-4
#funkcje
f1 <- function(x,a,b,c){
  return(a*x^2+b*x+c)
}
f2 <- function(x,b,c){
  return(b*x+c)
}
#dziedzina
dx<-seq(0,10,1)
#wartości dla prawdziwych parametrów
y1<-f1(dx,a1,b1,c1)
y2<-f2(dx,b2,c2)
#funkcja licząca niedopasowanie dla pierwszej funkcji
m1<-function(par){
  a<-par[1]
  b<-par[2]
  c<-par[3]
  mis1<-sum ( sqrt( (y1-f1(dx,a,b,c))^2 ) )
  return(mis1)
}
#funkcja licząca niedopasowanie dla drugiej funkcji
m2<-function(par){
  a<-par[1]
  b<-par[2]
  c<-par[3]
  mis2<-sum ( sqrt( (y2-f2(dx,b,c))^2 ) )
  return(mis2)
}
norma L2
Chodzi o znalezienie takich a b i c które
najlepiej pasują do realnych wartości
funkcji!
  #funkcja celu dla obydwu funkcji z wagami
  misfit<-function(par){
    a<-par[1]
    b<-par[2]
    c<-par[3]
    mis1<-sum ( sqrt( (y1-f1(dx,a,b,c))^2 ) )
    mis2<-sum ( sqrt( (y2-f2(dx,b,c))^2 ) )
    return(w1*mis1+w2*mis2)
  }
# zróbmy optymalizację z wagami
library(optimization)
#wagi
w1<-0.5
w2<-0.5
#algorytm startuje z punktu:
a<-0
b<-0
c<-0
#optymalizacja lokalna simplex
best<-optim_nm(start=c(a,b,c),k=3,fun=misfit)
best$par
#1.718751 3.074948 3.400393

#zróbmy multistart dla różnych losowych wag
#wektory i macierz na funkcje celu oraz na parametry:
m1_v<-rep(NA,1000)
m2_v<-rep(NA,1000)
par_m<-matrix(ncol=3,nrow=1000)
#1000 punktów startowych i 1000 wag:
for (i in 1:1000){
  #wagi sumują się do 1!!!
  w1<-runif(1)
  w2<- 1 - w1
  #losowe startowe a b i c
  a<-runif(1,0,5)
  b<-runif(1,0,5)
  c<-runif(1,0,5)
  best<-optim_nm(start=c(a,b,c),k=3,fun=misfit)
  m1_v[i]<-m1(best$par)
  m2_v[i]<-m2(best$par)
  par_m[i,]<-best$par
}
plot(m1_v,m2_v,pch=19)

#PSO
#inicjalizacja
#ilość ptaszków
n<-512
ba<-runif(n, min = 0, max = 5)
bb<-runif(n, min = 0, max = 5)
bc<-runif(n, min = 0, max = 5)
b<-data.frame(ba,bb,bc)
#budujemy ramkę z pozycjami i wartością funkcji celu
#apply bo wyliczamy wiersz po wierszu
z1<-apply(b,1,m1)
z2<-apply(b,1,m2)
b<-cbind(b,z1,z2)
#ustalamy który tak jest najlepszy dla obydwu funkcji
best_bird1<-which.min(b$z1)
best_bird2<-which.min(b$z2)

#pętla po iteracjach
for (k in 1:100){
  #pętla po ptaszkach
  for (i in 1:n){
    #odchylenia standardowe dla wymiarów a b i c
    sigma_a<-max(abs(b$ba[i]-b$ba[best_bird1]),abs(b$ba[i]-b$ba[best_bird2]))
    sigma_b<-max(abs(b$bb[i]-b$bb[best_bird1]),abs(b$bb[i]-b$bb[best_bird2]))
    sigma_c<-max(abs(b$bc[i]-b$bc[best_bird1]),abs(b$bc[i]-b$bc[best_bird2]))
    #testowe pozycje i wartość funkcji
    test_a <- rnorm(1, mean=(b$ba[i]+b$ba[best_bird1]+b$ba[best_bird2])/3 , sd=sigma_a)
    test_b <- rnorm(1, mean=(b$bb[i]+b$bb[best_bird1]+b$bb[best_bird2])/3 , sd=sigma_b)
    test_c <- rnorm(1, mean=(b$bc[i]+b$bc[best_bird1]+b$bc[best_bird2])/3 , sd=sigma_c)
    test_z1 <- m1(c(test_a,test_b,test_c))
    test_z2 <- m2(c(test_a,test_b,test_c))
    #jeśli jestem lepszy niż poprzedni robię hop
    if ( test_z1 <= b$z1[i] & test_z2 <= b$z2[i] ) {
      b$ba[i]<-test_a
      b$bb[i]<-test_b
      b$bc[i]<-test_c
      b$z1[i] <-test_z1
      b$z2[i] <-test_z2
    }
  } #ptaszki
  
  #który ptaszek jest teraz najlepszy?
  best_bird1<-which.min(b$z1)
  best_bird2<-which.min(b$z2)
} #iteracje
points(b$z1,b$z2,col="Red",pch=19)





