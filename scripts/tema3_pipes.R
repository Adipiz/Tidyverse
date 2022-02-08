
# Sesión 133: Pipes y cuándo usarlas


?magrittr

#Description
# The magrittr package offers a set of operators which promote semantics that will improve your code by

# structuring sequences of data operations left-to-right (as opposed to from the inside and out),

# avoiding nested function calls,

# minimizing the need for local variables and function definitions, and

# making it easy to add steps anywhere in the sequence of operations.

#* código que nos sirve para leer un código que sea más fácil de leer y entender


#* historia del conejo foo foo

# Little Bunny Foo Foo,
# Hopping through the forest,
# Scooping up the field mice,
# And bopping them on the head.

# recreando la historia del conejo
hop()
scoop()
bop()

# Variables intermedias

foo_foo1 <- hop(foo_foo, through = forest)
foo_foo2 <- scoop(foo_foo1, up = field_mice)
foo_foo3 <- bop(foo_foo2, on = head)


dd <- ggplot2::diamonds
dd1 <- dd %>% 
          dplyr::mutate(price_per_carat = price/carat)

# el problema de la creación de variables y la memoria
# * no hay que preocuparse ya que sólo se ocupará memoria en la medida que se cree una nueva variable con valores 
# * diferentes, es decir, dos variables con la misma info ocupa la misma memoria (es decir, no se duplica)
install.packages('pryr')



install.packages('Rcpp')
library(Rcpp)


?Rcpp


?object_size

library(pryr)
pryr::object_size(dd)


format(object_size(dd), units = "Mb")

format()













d = rnorm(20)
x <- rnorm(100)
x %<>% abs %>% sort   # el ejemplo es [some_object %<>% foo %>% bar <-> some_object <- some_object %>% foo %>% bar]
x
x <- rnorm(100)
x
x <- x %>% abs %>% sort
x
x %<>% abs %>% sort
x
df <- tibble::tibble(
a = rnorm(20),
b = rnorm(20),
c = rnorm(20),
d = rnorm(20),
e = rnorm(20)
)
df
df %>% View()
df$a <- (df$a - min(df$a, na.rm = TRUE))/(max(df$a, na.rm =TRUE)- min(df$a, na.rm = TRUE))
df$a <- (df$a - min(df$a, na.rm = TRUE))/(max(df$a, na.rm =TRUE)- min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE))/(max(df$b, na.rm =TRUE)- min(df$b, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE))/(max(df$c, na.rm =TRUE)- min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE))/(max(df$d, na.rm =TRUE)- min(df$d, na.rm = TRUE))
df$e <- (df$e - min(df$e, na.rm = TRUE))/(max(df$e, na.rm =TRUE)- min(df$e, na.rm = TRUE))
View(df)
x <- df$a
rng <- range(x, na.rm = TRUE)
rng
x
rng[1]
# seguimos reemplazando
(x - rng[1])/(rng[2]- rng[1])
# Ahora vamos a crear una función que me permita reescalar los valores
rescale_0_1 <- function(x){
rng <- range(x, na.rm = TRUE)
(x - rng[1])/(rng[2]- rng[1])
}
rescale_0_1(df$a)
rescale_0_1(df$b)
rescale_0_1(df$c)
rescale_0_1(c(0,5,1))
rescale_0_1(c(0,5,10))
rescale_0_1(c(0,5,10))
df$a <- rescale_0_1(df$a)
df$b <- rescale_0_1(df$b)
df$c <- rescale_0_1(df$c)
df$d <- rescale_0_1(df$d)
df$e <- rescale_0_1(df$e)
rng <- range(x, na.rm = TRUE, finite = TRUE)
x <- c(1:10, Inf)
x
rescale_0_1(x)
# Ahora vamos a crear una función que me permita reescalar los valores
rescale_0_1 <- function(x){
rng <- range(x, na.rm = TRUE, finite = TRUE)
(x - rng[1])/(rng[2]- rng[1])
}
rescale_0_1(x)
library(tidy)
library(tidyverse)



# sesión 138: condicioanles 


# Condicionales ------------------------

if (condicion){
  # código a ejecutar si la condición es TRUE
} else {
  # código a ejecutar si la condición es FALSA
}

?`if`


 # 
  has_name <- function(x){
    nms <- names(x)
    if(is.null(nms)){
      # no existe el objeto en cuestión
      rep(FALSE, length(x))
    } else {
      
      # ausencia de nas y de ""
      !is.na(nms) & nms != ""
    }
  }

a <- c('hola',2,3)
length(a)


has_name(tribble(
  ~x, ~y, ~` `,
   1,  2,   3
))


# && -> AND
# || -> OR

if(any(c(T,F))){
  
  'tenemos al menos un verdadero'
} else {
  
  'no hay ningún verdadero'
}

if (all(c(T,F))){
  'Tenemos todas las condiciones verdaderas'
} else{
  'tenemos alguna condición falsa'
}

# cosas que parecieran ser iguales 
identical(0, 0L)

2 == sqrt(2)^2

# función para evaluar si dos número están lo suficientemente cerca
dplyr::near(2, sqrt(2)^2)

# comparar con NAs resulta en NAs


# Uso de la condición if

if(condicion){
  # resultado 1
} else if(condicion2){
  # resultado 2
} else if (condicion3){
  # resultado 3
} else{
    # resultado por defecto
  }
    
# nueva forma de escribir una función

 calculate <- function(x,y, op){
  switch(op,
        suma  = x + y,
        resta = x - y,
        multiplicación = x*y,
        division = x/y,
        stop('Warning: error en la ejecución de la operación o la operación no está identificada') )
}

 
  calculate(2,3, 'suma')
  calculate(5, 7.2, 'multiplicación')
  calculate(5, 7.2, 'resta')

  
  temperatura <- -5

  if(temperatura < 0 && length('Hace sol') > 0) {
    message('es negative con este sol que hace')
  }  
  
  
  y <- -7
  
  x <- 5
  if( y <= 0){
    log(x)
  } else{
    y^x
  }
  
  # recomendaciones con el uso de llaves. Se favorece prescindir de ellas cuando el código es corto 
  
  y <- 10
  x <- if(y<20) 'Número pequeño' else 'Número grande'
  

# Argumentos --------------------------------------------------------------

  
  # Sesión 139: Los argumentos de las funciones 
  
   # argumentos en dos grupos; de dato o detalle del cálculo
  
   # Dato
   # Detalle del cálculo
  
  ?mean
  log(x = 8, base = 2)
  mean(x = c(1,2,3,4,5,10,11), trim = 0.5, na.rm = TRUE)
  
  t.test(rnorm(100, mean = 0, sd = 1), rnorm(100, mean = 1, sd = 1), alternative = 'greater', paired = T,
         var.equal = t, conf.level = 0.99)
   
  stringr::str_c(c('banana', 'manzana', 'pera'), collapse = ', ')
  
       
  
  standar_ci <- function(x, conf = 0.95){
    se <- sd(x)/sqrt(length(x))
    alpha <- 1 - conf
    mean(x) + se*qnorm(c(alpha/2, 1-alpha/2))
  }
  
  standar_ci(runif(1000))
  
  standar_ci(runif(1000), conf = 0.99)
  
  standar_ci(runif(1000), conf = 0.99999) 
  
  
  plot((runif(100)))
  plot(rnorm(100))
  
  # dato primero de las variables
  # cuidado con los parámetros
  # uso correcto de comas y espacios en blanco para la lectura
  
  
  # vectors en argumnetos de una fn x, y, z
  # vector de pesos w
  # df, data, d : data frame
  
  # subíndices numéricos i, j, k
  # n: longitud de un vector
  # m : número de columnas
  # p : probabilidades
  
  # Sesión 140: punto, punto y punto y evaluación tardía.
  
  
  # comprobando el argumento de algunas funciones
  
   # * Calculando la media ponderada en una fn
  
  # La noción de promedio ponderado se utiliza para nombrar a un método de cálculo que se aplica cuando, 
  # dentro de una serie de datos, uno de ellos tiene una importancia mayor. Hay, por lo tanto, un dato
  # con mayor peso que el resto. El promedio ponderado consiste en establecer dicho peso,
  # también conocido como ponderación, y utilizar dicho valor para realizar el cálculo del promedio.

  
  ??call.

  wt_mean <- function(x,w){
      if(length(x) != length(w)){
        stop("'x' y 'w' no tienen la misma longitud", call. = FALSE)
      }
    sum(x*w) / sum(w)
  }
  
    # varianza ponderada
    wt_var <- function(x,w){
      mu <- wt_mean(x,w)
      sum(w*(x-mu)^2)/sum(w)
    }
  
    
    # sd ponderada
    
    wt_sd <- function(x,w){
      sqrt(wt_var(x,w))
    }
  
    # posible fuente de errores
    wt_mean(1:6, 2:7)
    
    # podemos agregar un mensaje en caso que las longitudes sean diferentes
    
    #--
    
    a <- c(1,2,4:9, NA, 12:18)
    
    a<- a[!is.na(a)]
    
    b <- TRUE
    is.logical(b)

  
  
    # Un algoritmo más cool
    
    
    wt_mean <- function(x,w, na.rm = TRUE){
      stopifnot(is.logical(na.rm),
                length(na.rm) == 1,
                length(x) == length(w)
      )
      if(na.rm) {
        missing <- is.na(x) | is.na(w)
        x <- x[!missing]
        y <- y[!missing]
      }
      
      sum(x*w) / sum(w)
    }
  
    
    wt_mean(1:6, 2:7, na.rm = TRUE)
  
  
    # ... número arbitrario de parámetros
    sum(1)
    str_c(c('a', 'b', 'c'))
    
    commas <- function(...) stringr::str_c(..., collapse = ", ")
    
    commas(c('a','b'))
    commas(letters[1:10])
    
    
    ??pad
    # regla general
    
    rule <- function(..., pad = ' '){
      title <- paste0(...)  # concatena vectores luego de convertirlos a caracter
      width <- getOption('width') - nchar(title) - 5
      cat(stringr::str_dup(pad, width/2), ' ', title, ' ', stringr::str_dup(pad, width/2) )
      
      }
    
    rule('Sección 1 ', 'Sección 2', ' Sección 3')
    
    sum(c(1,2), na.mr = T) # cuidad con estos errores de typing
    
    
    # Sesión 141: Valores de retorno y valores de entorno
    
    #uso de return siempre luego de finalizar
    
    
    my_function <- function(x,y,z){
      if(length(x) == 0 || length(y) == 0){
        return(0)
      }
      
      # El código sigue adelante...
    }
    
    # transformación : el objeto de entrada es modificado antes de ser devuelto
    # efecto secundario: el objeto de entrada no es modificado (plot, write)
    
    
    show_nas <- function(df){
      n <- sum(is.na(df))
      cat('Número de NAs: ', n, '\n', sep ='')
      
      invisible(df)   # hace una copia del data frame que tenemos como argumento
          }
    
    
   x <- show_nas(diamonds)
    
    class(x)
    
    dim(x)
    
    mtcars %>% 
      show_nas() %>% 
      mutate(mpg = ifelse(mpg>30, NA, mpg)) %>% 
      show_nas
    
    # entorno donde se ejecuta
    
    f <- function(x){
      x + y
    }

    y <- 5    # cuidado con esto, este argumento no está en el argumento de la función f
 f(3)    
              # definir bien la función
 
 
 # creemos la función suma
 
 `+` <- function(x,y){
   if(runif(1)< 0.1){
     sum(x,y)
   } else{
     sum(x,y)*1.5
   }
 }
    x <- table(replicate(1000, 2 + 3))
    class(x)
  table(replicate(1000, 2 + 3))
 
 # removemos esta función
  
  rm(`+`)
 
  
 

# vectores ----------------------------------------------------------------

  # Sesión 142: la estructura del vector
  
  # Vectores atómicos: lógic, entero, double (numeric), character, complex, raw 
    # * este tiene un sólo tipo de datos
  # listas : vectores recursivos y heterogéneos 
  # NA -> ausencia de un valor dentro de un vector
  # NULL -> ausencia del vector, es como un vector de longtitud 0
  
  
  typeof(letters)
  typeof(1:10)
  
  length(letters)
  x <- list('a', 'b', 1:10)  
  
  typeof(x)
  length(x)
  
  # vectores auemntados
  # factor: vector aumentado sobre los enteros
  # date y date-time: vector auemntado sobre vectores numéricos
  # date frame y tibble: vector sobre listas.
   
  # vectores atómicos
  
    # lógicos: solo piuede tomar estos tres valores TRUE; FALSE; NA
  
  1:10 %% 3 == 0
   
  c(TRUE, FALSE, TRUE, FALSE)
  
  # numérico: es el más común
  
  typeof(1)   # entero
  typeof(1L)  # double, una aproximación y no un valor real
  
  # para comparar con una distancia pequeña
  
x <- sqrt(2)^2

x -2

dplyr::near(x, 2)
  
  # double Na, NaN, inf, -Inf
  x<- c(-1,0,1, NA, 2)/0
  is.finite(x)
  is.infinite(x)  
  is.na(x)
  is.nan(x)  
  
  
  # character
    #*
  
  x <- "Dábale arroz a la zorra el abad"

  format(object.size(y))  
  
  y <- rep(x, 1000)

  format(object.size(y), units = "auto")
  ?object.size
  
  
 # pointer -> 8 bytes 
  
  (8 * 10000 + 152)/1000/1024

  # número complejo
  
  1 + 5i
  
  NA # LÓGICO
  NA_character_ # character
  NA_complex_   # complejo
  NA_integer_   # entero
  NA_real_      # double
  
  

# Castings ----------------------------------------------------------------

  # Sesión 144: Transformaciones y castings
  
  as.logical(c(1,0,0,0,1))
  as.integer(c(T,F,F,F,T))
  as.double(c(1,2,3))  
  as.character(c(1,21,3))
  
  
  x <- sample(20, size = 100, replace = TRUE)
  y <- x > 10
  
  sum(y)   # elementos mayores a 100
  mean(y)  # qué proporción son mayores a 10  
  
  if(length(x)){ # si la longitud es 0 se convierte a FALSE, de lo contrario es TRUE
    # funionalidad
    # * pero esa sintaxis no es muy clara
  }
  
  typeof(c(TRUE, 1L))
  typeof(c(1L, 1.6))
  typeof(c(1.6, 'a'))  
  
  # vector atómico es un solo tipo de dato
  
  
  is_logical(c(T,T,T))         # solo cierto para valores lógicos
  is_integer(c(1L,2L,3L))      # int
  is_double(c(1,2,3))          # dbl
  is_numeric(c(1,2,3,4L))      # int, dbl
  is_character(c('a', 'b', 3L)) # cha
  is.atomic(c(T,T,3, 'b'))      # lgl, int, dbl, cha
  is.vector(c(T,T))             # cualquiera de los 5 datos    

# Recycling rule ----------------------------------------------------------
  # Sesión 145: la regla del reciclaje de datos 
  
  sample(10) + 12
  runif(10) > 0.5  
  
  # suma dos vectores de diferente longitud
  
  1:10 + 1:2 # se copia el vector 1:2 de tal manera que cupa en el vector 1:10
  1:10 + 1:3 # longitud de objeto mayor no es múltiplo de la longitud de uno menor
  1:10 + 1:5 
  
  # qué ocurre si creamos una tibble
  
  tibble(
    x = 1:4,
    y = 1:2
  )                        # genera error
  
  
  # formas alternativas correctas
  
  tibble(
    x = 1:4,
    y = 1
  )
  
  tibble(
    x = 1:4,
    y = 1
  ) 
  
  tibble(
    x = 1:4,
    y = rep(1:2,2)
  )

  
  tibble(
    x = 1:4,
    y = rep(1:2, each = 2)
  )  
    
  # crear vector con nombres
  
  c(x = 1, y = 2, z =3)
  
  
  # o alternativamente
  
  set_names(1:3, c('x', 'y', 'z'))
  

# Sesión 146: Subconjuntos de vectores ------------------------------------

  # subsetting
  # filter solo aplicable a tibbles
  
  # tipos de filtrados
  x[5]
  
  x <- letters
  x[c(3,2,6)]  # posiciones 3, 2 y 6
  x[c(1,1,7,7,7,7,3,3,3)] # queremos repetir más de una vez
  x[c(-3,-5,-7)] # queremos eliminar elementos del vector, recuerda no mezclas + y -
  x[-c(6:15)] # elimina las posiciones 6 a 15
  
  
  # nos quedamos con todos los vecrtores que tengan TRUE
  
   # Comparaciones lógicas
  x <- c(4,5,8, NA, 2,1,3,NA)
  x[!is.na(x)]
  x[x %% 2 == 0]
  
  
  
  # vector con nombre
  
  
  x <- c(abc = 1, def = 2, ghi = 3)
  x[c('def', 'abc', 'abc')]  


  x <- matrix(1:9, byrow = T ,ncol = 3)

  x[1,]    # tomo la primera fila de la matriz x
  x[,1]    # tomo la primera columna de la matriz
  x[-1, ]  # tomo toda la matriz menos la primera fila
  x[,-1]   # tomo toda la matriz menos la primera columna
 
  # doble corchete para encontrar un elemento
  
  x['abc']  # toda la columna es selecciondada
  x[['abc']] # el eleemnto es seleccionado
    
   # Cuidado que el uso de corchetes en listas es diferente que en vectores
  
  

# Sesión  147: Los vectores recursivos ------------------------------------

  # Listas
  
  x <- list(x  = 1, y = 2, z = 3)
  
  str(x)
    
    # la lista puede contener un mix de objetos
  
  x <- list('x', pi, TRUE)

  str(x)  
   
    # también podemos tener listas que están hechas de listas
  
  x <- list(list(1,2,3), list(pi, sqrt(2)), list('a', 'b', 0))
  str(x)  
  
  
    x1 <- list(c(1,2), c(3,4))
  str(x1)
  
  
    x2 <- list(list(1,2), list(3,4))
  str(x2)
  
  x3 <- list(1, list(2, list(3)))
  
  
  x <- list(
    a = 1:3,
    b = "soy un string",
    c = pi,
    d = list(-2,-5)
  )
  
  # cómo vamos a una sublista
  
  x[1:2]
  
  str(x[1:2])
  
  x[4]
  str(x[4])
  
  # si me quiero quedar solo con una línea de la lista
  
  x[c('c', 'a')]
  
  x[[1]] # esta selección resulta en el primer elemento de la lista
  
  str(x[[1]])
  
  
  # si quiero los valores exactos de elemntos en una lista
  
  x[['a']]
  x$a
  
  # si me interesa el valor como tal 
  
  x[[4]][1]
  x[[4]][2] 
  
  x[[4]][[1]]
  x[[4]][2] 

# Sesión 148: Atributos de un vector --------------------------------------

  x <- 1:12
  
  ?attr  # Get or set specific attributes of an object
  
  attr(x, 'desc')
  attr(x, 'desc') <- 'Vector de las horas del día'
  attr(x, 'desc')
  attr(x, 'created') <- 'Alonso Pizarro'
  attr(x, 'source') <- 'Curso Tidy'

  attributes(x)
    
  
  as.Date
  methods("as.Date")
  getS3method('as.Date', 'character')
  getS3method('as.Date', 'numeric')
  
  methods('print')

# Sesión 149: Vectores Aumentados -----------------------------------------

  
  # factor
  
  x <- factor(c('L', 'M', 'S', 'D'),
              levels = c('L', 'M', 'X', 'J', 'V', 'S', 'D'))
  
  x
typeof(x)  
  
  attributes(x)
  
  
  x <- as.Date('1988-05-19')
  
  typeof(x)
  attributes(x)
  unclass(x)  
  
  
  
  #POSIXct
  
  x <- lubridate::ymd_hm('19880519 16:30')
  x
  typeof(x)  
  unclass(x) # número de segundos después del EPOCH q de enero de 1970
  attributes(x)
  attr(x, 'tzone') <- 'US/Pacific'
  x
  attr(x, 'tzone') <- 'US/Eastern' 
  x

  
  #POSIXlt
  
  y <- as.POSIXlt(x)
  typeof(y)
  attributes(y)

  attr(y, 'names')  
  
  
  
  
  z <- lubridate::as_datetime(y)
 typeof(z)  
  
 # Tibble
 tb <- tibble(
   x = 1:5,
   y = 6:10
 )
typeof(tb) 
attributes(tb) 


  # diferencia entre una tibble y una lista. tibble require que los vectores sean de la misma longitud
    # la lista es heterogénea

df <- data.frame(x = 1:5,
                 y = 6:10)
typeof(df)
attributes(df)





# Sesión: el bucle for para iterar ----------------------------------------

  # * programación imperativa y funcional

  # bucle for

t <- tibble(
  a= rnorm(100),
  b= rnorm(100),
  c= rnorm(100),
  d= rnorm(100),
  e= rnorm(100)
  
)

t %>% View()

median(t$a)
median(t$b)
median(t$c)
median(t$d)
median(t$e)

t[[1]]

?seq_along

output <- vector('double', ncol(t))  # creamos un vector vacío, variable de resultado salida

seq_along(t) # 1:ncol(t)


for(i in seq_along(t)){              # evitar crecer el array dentro porque es menos eficiente ; secuencia
  output[[i]] = median(t[[i]])        # cuerpo
}

output



# Sesión 151: Más tipo de bucles ------------------------------------------

    # Variantes del bucle for

    # Modificación de objetos



df <- tibble(
  a= rnorm(20),
  b= rnorm(20),
  c= rnorm(20),
  d= rnorm(20),
  e= rnorm(20))
  


rescale_0_1 <- function(x){
  rng <- range(x, na.rm = TRUE)
  (x - rng[1])/(rng[2]- rng[1])
}


df$a <- rescale_0_1(df$a)
df$b <- rescale_0_1(df$b)
df$c <- rescale_0_1(df$c)
df$d <- rescale_0_1(df$d)
df$e <- rescale_0_1(df$e)


for(i in seq_along(df)){
  df[[i]] <- rescale_0_1(df[[i]])
}

df

    # Patrones de bucle
# por posición   # este es el más usado
# for(i in seq_along(df)) -> x[[i]]

#    por elementos
#form(x in df)


# por nombre
#for(name in names(df)) ->df[[name]]

df

results <- vector('list', length(df))

names(results) <- names(df)

for(i in seq_along(df)){
  
  name <- names(df)[[i]]
  value <- df[[i]]
  print(paste0(name, ' - ', value))
}


# desconocimiento de la longitud de output

  means<- 0:10000
  
  # output <- double() # valor numérico 
  output<- vector('list', length(means))
  
  for(i in seq_along(means)){
    n <- sample(100, 1)
    # output <- c(output, rnorm(n, means[[i]]))  # recuerda reiniciar el vector
    output[[i]] <- rnorm(n, means[[i]])
     }

  str(output)
  
  
  # deslistamos la lista para convertirla en un vector
  
  str(unlist(output))
  
  
  
  lista <- vector('list', 4)
  
  lista[[1]] <- rnorm(2, 55)
  lista[[2]] <- rnorm(3, 55)
  lista[[3]] <- rnorm(4, 55)
  lista[[4]] <- rnorm(5, 55)
  
  # deslistamos 
  
  listal <- unlist(lista) # aplanar la lista de versiones
  
  
  # vector ----
  vector <- double() # vacío
  vector <- c(vector, 1)
  vector <- c(vector, 2)
  vector <- c(vector, 2)
  vector <- c(vector, rnorm(2))
  class(vector)
  
  seq_along(vector)
  
  
  
  purrr::flatten_dbl(lista) # aplanar solo los datos dbl 
  purrr::flatten_dbl(output)
  
  # nota que trabajr con listas en los bucles
  
  
  
  # uso de rbind(), cbind() 
  
  
  dplyr::bind_cols()
  dplyr::bind_rows()
  
  
  
  
  # desconocimiento de la longitud de la iteración
     # ej: una simulación, lanzamiento de monedas
  
  # usamos elbucle while, este es una forma más genérica
  
  
  while(condition){ # condition es una condición booleana
    # cuerpo del bucle
    #.
    #.
    #.
    #---
    # dentro del bucle, en algún moemnto condición se vuelev FALSE
    }
  
  # podemos crear la misma instrucción con for
  
  
  for(i in seq_along(df)){
    # body
    print(names(df)[[i]])
  }
   
  i <- 1
  while(i <= length(df)){
    # body
    print(names(df)[[i]])
    
    i <- i + 1
  }  
  
  
  flip_coin <- function(){
    sample(c('C', 'X'),1)
  }
  
  count_heads <- function(total_head){
  flips <- 0
  n_heads <- 0
  
  while(n_heads < total_head){
    if(flip_coin() == 'C'){
      n_heads <- n_heads + 1
    } else {
      n_heads <- 0
    }
    
    flips <- flips + 1
  }
  
  flips
  }
  
  count_heads(8)

# Sesión 152: Bucles vs programación funcional ----------------------------
  # R es programación funcional
  
  
  
  df <- tibble(
    a= rnorm(20),
    b= rnorm(20),
    c= rnorm(20),
    d= rnorm(20),
    e= rnorm(20))
  
  
  col_means <- function(df){
  output <- vector('double', length(df))
  
  for(i in seq_along(df)){
    output[[i]] <- mean(df[[i]])
  }
  output
  }
  
  
  col_means(df)
  
  
  
  
  col_medians <- function(df){
    output <- vector('double', length(df))
    
    for(i in seq_along(df)){
      output[[i]] <- median(df[[i]])
    }
    output
  }
  
  
  col_medians(df)
  
  
  
  col_sd <- function(df){
    output <- vector('double', length(df))
    
    for(i in seq_along(df)){
      output[[i]] <- sd(df[[i]])
    }
    output
  }
  
  
  col_sd(df)
  
  
  # funciones 
  
  
  f1 <- function(df) abs(df-mean(df))^1
  f2 <- function(df) abs(df-mean(df))^2
  f3 <- function(df) abs(df-mean(df))^3

  # una forma más general
  
  
  
  f <- function(df, i = 1) abs(df-mean(df))^i
  
  
  # teniamos estructuras similares. en el saco de mean, median y sd puedo reducir el código
  # de tal forma que
  
  col_summary <- function(df, fun){
    output <- vector('double', length(df))
    
    for(i in seq_along(df)){
      output[[i]] <- fun(df[[i]])
    }
    output
  }

  
  col_summary(df, mean)
  col_summary(df, min)
  col_summary(df, max)
  col_summary(df, median)
  col_summary(df, sd)
  # fíjate que el parámetro es una función
  
  
  # paquete base de R
  apply()
  lapply()
  sapply()
  tapply()

    # un elemnto de la lista -> (purrr) -> todos los elementos de la lista
    # resolver pequeños problemas que se unan en cojunto con una pipe
  
  

  # Sesión 153: las función de map ------------------------------------------
    # están programadas en c (difícil de leer), son más eficientes
  
  ?map() # crea una lista
  map_lgl() # crea un vector lógico
  map_int()  # crea un vector de enteros
  map_dbl()  # crea un vector de dbls
  map_chr() # crea un vectior de caracteres
  
  # estructura de datos más elegante
  
  map_dbl(df, mean) 
  map_dbl(df, median) 
  map_dbl(df, sd)
  
  
  df %>% map_dbl(mean)
  df %>% map_dbl(median)
  df %>% map_dbl(sd)
  
  df %>% map_dbl(mean, trim = 0.5)
  
  # para listas
  
  z <- list(x = 1:5, y = 6:10)
  map_int(z, length)  
  
  # modelo lineal
  
  mtcars
  
  models <- mtcars %>% 
    split(.$cyl) %>% 
    map(function(df) lm(mpg ~ wt, data = df))
  
  
  models <- mtcars %>% 
    split(.$cyl) %>% 
    map(~lm(mpg ~ wt, data = .))
  
    
  ?mtcars
  
  
  # para acceder a alguna variable
  models %>% 
    map(summary) %>% 
    map_dbl(~.$r.squared)
  
  models %>% 
    map(summary) %>% 
    map_dbl('r.squared')
  

  x <- list(list(1,2,3), list(4,5,6), list(7,8,9))  
  
  x %>% 
    map_dbl(2)
  
  # map() <-> lapply()
  # sapply() 
  
  
  x1 <- list(
    runif(5),
    runif(5),
    runif(5)
  )
  
  x2 <- list(
    runif(5)/2,
    runif(5)/2,
    runif(5)
  )
x1  
x2  
  

threshold <- function(x, cutoff = 0.75){
  x[x>cutoff]
}

x <- c(0, 0.88, 0.76, 0.55)
  
  threshold(x)

  
  ?sapply

  x1 %>% sapply(threshold) %>% str()
  
  x2 %>% sapply(threshold) %>% str()
  
  
  #vapply(df, id.numeric, logical(1))
  #map_lgl(df, is.numeric)
    
  
  

# Sesión 154: Las funciones Safely, Possibly, Quietly ---------------------

  safe_log <- safely(log)              
  
  str(safe_log(12))
  
  str(safe_log('antonio'))
  
  
  x <- list(1, 10, 'z', -8)
  
  
  x %>% map(safe_log) %>% transpose %>%  str()
  
  
  x %>% map(safe_log) %>% transpose() -> y
  
  
  y$error %>% map_lgl(is_null) -> is_ok
 
  x[!is_ok]  
  
  y$result[is_ok]  %>%  flatten_dbl()
  
  
  
  x %>% map_dbl(possibly(log, NA_real_))
  
  
  list(5,-5) %>% map(quietly(log)) %>%  str()
  
  
  

# Sesión 155: Mappings con argumentos múltiples ---------------------------
  mu <- list(2,17,-5)

  mu %>% 
    map(rnorm, n = 10) %>% str()
    
  sigma <- list(1, 5, 25)
  seq_along(mu)

  seq_along(mu) %>% # . vale 1, 2,3, espectivamente
    map(~rnorm(10, mu[[.]], sigma[[.]])) %>% 
    str()
  
  
  
  map2(mu, sigma, rnorm, n = 10) %>%  str()
  
  
  map2 <- function(x, y, f, ...){
    out <- vector('list', length(x))
    for (i in seq_along(x)){
      out[[i]] <- f(x[[i]], y[[i]], ...)
    }
  }
  
  
  n <- list(10, 15, 22)

  args <- list(n, mu, sigma)   
  
  args %>%  pmap(rnorm) %>% 
    str()
  
  # cuidado con los nombres ya que hay que mantener el orden si no los asignamos correctamente
  
  args_ok <- list(mean = mu, sd = sigma, n = n )

  args_ok %>% pmap(rnorm) %>% str()
    
 ?list 
  
  
  param <- tribble(
    ~mean,     ~sd,     ~n, 
        2,       2,     10,
       17,       5,     15,
       -5,      25,     22
  )
  
  
  param %>%  pmap(rnorm)



  
  
  f <- c('runif', 'rnorm', 'rpois')
  
  param <- list(
    list(min = -5, max = 5),
    list(sd = 3),
    list(lambda = 12)
  )
  
  
  invoke_map(f, param, n = 15) %>%  str()
  
  
  matchs <- tribble(
    ~f,     ~param,
    'runif', list(min = -5, max = 5),
    'rnorm', list(sd = 5),
    'rpois', list(lambda = 12)
  )
  
  matchs %>%  mutate(sim = invoke_map(f, param, n = 15)) %>% View()
  
  
  
  

# Sesión 156: Otras funciones ---------------------------------------------

  
  
  x <- list('hola', 123, -pi)
  x %>% walk(print)
  
  # para guardar graficos en una lista
  
  
  
  plot <- mtcars %>% 
         split(.$cyl) %>% 
    map(~ggplot(., aes(mpg,wt)) + geom_point())
  
  paths <- str_c(names(plot), ".pdf")
  
  
  pwalk(list(paths, plot), ggsave, path = tempdir())
  tempdir()
  ?ggsave
  
  # si sólo quiero quedarme con los factores
  iris %>% keep(is.factor) %>% str()
  
  # si quiero descartar 
  
  iris %>%
    discard(is.factor) %>%  str()
  
  # some y every
  
  x <- list('a' = 1:5, letters, list(16))
  
  x %>% some(is_character)
  x %>% every(is_character)

  x <- sample(12)
    
  x %>% detect(~. < 7) # primer elemento menor quee...
  
  x %>% detect_index(~. < 7) # posición
  
  
  
  x %>%  head_while(~. > 7) # me quedo con los número mayores a 7 qye conforman la cabeza de los datos
  x %>%  tail_while(~. < 7) # lo mismo pero en la cola de los daros y menores
  
  dfs  <- list(
    age = tibble(name = 'Alonso Pizarro', age = 30),
    sex = tibble(name = c('Alonso Pizarro', 'July'), sex = c('M', 'F')),
    trt = tibble(name = 'July', treatment = 'Mrs')
  )
  
  dfs %>% reduce(full_join)
  
  
  vs <- list(
    1:5,
    c(1,3,5,8,10),
    c(1,2,3,7,8,10),
    c(1,2,4,6,9,10))
  
  
  vs %>% reduce(intersect)
  
  x <- sample(12)
  
  
  x %>% accumulate(`+`)  # accumulate le aplicamos una función acumulada
  
  
  
  x <- (c(5,2,3,6,1,0.2))
  which(x == min(x))  
  