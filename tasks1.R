# Klaudia Faltyn 112101

# Zadanie 1.

heritability1 <- function(var_a, var_p) {
  if(var_a <0) stop("Wariancja genetyczna nie mo¿e byæ ujemna")
  if(var_p <0) stop("Wariancja fenotypowa nie mo¿e byæ ujemna")
  if(var_p < var_a) stop ("Wariancja genetyczna nie mo¿e byæ wiêksza od fenotypowej")
    h2 = var_a/var_p
      return(h2)
    }
heritability1(1, 5)


# Zadanie 2.

heritability2 <- function(h2=NULL, var_a=NULL, var_p=NULL){
  if (sum(sapply(list(h2, var_a, var_p), is.null)) >1) stop("Tylko jedna z wartoœci mo¿e byæ nieznana")
  if(!is.null(var_a)) if(var_a < 0 ) stop("Wariancja genetyczna nie mo¿e byæ mniejsza od 0.")
  if(!is.null(var_p)) if(var_p < 0) stop("Wariancja genetyczna nie mo¿e byæ mniejsza od 0.")
  if(!is.null(var_a) & !is.null(var_p)) if (var_p < var_a) stop("Wariancja genetyczna nie mo¿e byæ wiêksza od wariancji fenotypowej.")
  if (!is.null(h2)) if(h2 < 0 | h2 > 1 ) stop("odziedziczalnoœæ musi mieæ wartoœæ z przedzia³u [0,1]")
  
  if (is.null(h2)) h2 = var_a/var_p
  if (is.null(var_a)) var_a = h2*var_p
  if (is.null(var_p)) var_p = var_a/h2
  
  return (list("odziedziczalnosc" = h2, "wariancja_genetyczna" = var_a, "wariancja_fenotypowa" = var_p))
  }
heritability2(var_p=5, var_a=1)


# Zadanie 3.

gen_progress <- function(P0,Pr,T,h2,var_a,var_p){
  r = (Pr-P0)
  if ((r <= 0 )) stop("Pr-P0 musi byæ wiêksze od zera ")
  if (var_a < 0) stop("Wariancja nie mo¿e byæ mniejsza od zera!")
  if (var_p < 0) stop("Wariancja nie mo¿e byæ mniejsza od zera!")
  if(var_p < var_a) stop ("Wariancja genetyczna nie mo¿e byæ wiêksza od fenotypowej")
  h = sqrt(h2)
  a = sqrt(var_a)
  s = r/sqrt(var_p)
  DA = s*h*a
  AP = DA/T
  print (structure(list("standaryzowana ró¿nica selekcyjna" = s,"postêp hodowlany" = DA,"postêp hodowlany na 1 rok`"=AP)))
}
gen_progress(P0 = 148.8, Pr = 157.49, T = 3, h2 = 0.49, var_a = 122.3236, var_p = 249.64)


# Zadanie 4.

gen_progress <- function(P0,Pr,T,h2=NULL,var_a=NULL,var_p=NULL){	
  r = (Pr-P0)
  if ((r <= 0 )) stop("Pr-P0 musi byæ wiêksze od zera ")
  if (sum(sapply(list(h2, var_a, var_p), is.null)) > 1) stop("Tylko jedna z wartoœci mo¿e byæ nieznana")
  if(!is.null(var_a) & !is.null(var_p)) if (var_p < var_a) stop("Wariancja genetyczna nie mo¿e byæ wiêksza od wariancji fenotypowej.")
  if(!is.null(var_a)) if(var_a < 0 ) stop("Wariancja genetyczna nie mo¿e byæ mniejsza od 0.")
  if(!is.null(var_p)) if(var_p < 0) stop("Wariancja genetyczna nie mo¿e byæ mniejsza od 0.")
  if (!is.null(h2)) if((h2 < 0) | (h2 > 1)) stop("Odziedziczalnoœæ musi mieœciæ siê w przedziale od 0 do 1.")

  x = heritability2(h2, var_a, var_p)
  
  if (x$wariancja_genetyczna <= 0) stop("Wariancja genetyczna nie mo¿e byæ mniejsza od zera!")
  if (x$wariancja_fenotypowa <= 0) stop("Wariancja fenotypowa nie mo¿e byæ mniejsza od zera!")
  
  h = sqrt(x$odziedziczalnosc)
  a = sqrt(x$wariancja_genetyczna)
  s = r/sqrt(x$wariancja_fenotypowa)
  DA = s*h*a
  AP = DA/T
  print (structure(list("standaryzowana ró¿nica selekcyjna" = s,"postêp hodowlany" = DA,"postêp hodowlany na 1 rok`"=AP, "odziedziczalnoœæ" = x$odziedziczalnosc, "wariancja genetyczna" = x$wariancja_genetyczna, "wariancja fenotypowa" = x$wariancja_fenotypowa)))
}
gen_progress(P0 = 148.8, Pr = 157.49, T = 3, var_p=249.64, var_a = 122.3236)

