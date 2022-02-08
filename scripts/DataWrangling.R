getwd()
## Vamos a usar el paquete TIBBLE data frames mejor estructurados

vignette("tibble")

#una tibblee es un data frame, pero un data frame no siempre es una tibble

View(iris)

class(iris)

iris_tibble <- as_tibble(iris) # Crear un objeto de tipo tibble

class(iris_tibble)


# creando una tibble

t <- tibble(
  x = 1:10,
  y = pi,
  z = y * x^2
    )

t[2,3] # segunda fila, terecera columna.NOMBRE NO SINT{ACTICOS}


t1 <- tibble(
  `:)` = "smile",
  ` `  = "space",
  `1988` = "number"
)


# creando un tribble, creando una tibble por filas

tribble(
  ~x , ~y, ~z,
  "a" , 1 , 3.14,
  "b" , 2 , 6.28,
  "c" , 3 , 9.42
  
)

tibble(
  a = lubridate::now() + runif(1e3)*24*60*60,
  b = 1:1e3,
  c = lubridate::today() + runif(1e3)*30,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = T)
)


# visualizando con print de las tibbles

nycflights13::flights %>% 
  print(n = 15, width = Inf)

options(tibble.print_max = 12, tibble.print_min = 20) # de 20 quiero ver 12
options(dplyr.print_min = Inf)  # quiero ver todas las filas de informacion
options(tibble.width = Inf)   # quiero ver todas las columnas de la tibblr

# muchas más opciones

package?tibble

# si quiero visualizar la data completa

View()

#[[´nombre_variable´]]
#[[posicion_variable]]
#$nombre_variable

df <- tibble(
  x = rnorm(10),
  y = runif(10)
)

# muestra los valores que contienen las filas x e y
df$x
df$y

# con pipe

df %>% .$x
df %>% .$y


df[["x"]]
df[["y"]]

df %>% .[["x"]]
df %>% .[["y"]]


df[[1]]
df[[2]]

df %>% .[[1]]
df %>% .[[2]]


# hay funciones que no están programadas para tibble
# corchetes no funcionan para data rames
#[[]]
dplyr::filter()
dplyr::select()

# [[]] sobre un df, puede devolver un data.frame o un vector
#[[]] sobre una tibble, siempre devuelve una tibble


#ASSIGNMENT



# ¿Tibble o no tibble? ¿Cómo sabes si un objeto es una tibble o no?
    # Puedo aplicar el función class() para ver si el objeto es una tibble o no
#por ejemplo
  class(nycflights13::flights)
    # vemos que es "tbl_df"     "tbl"        "data.frame"
  
  # también podemos imprimirla con directamente
  
  nycflights13::flights 
  
  
  
  # en este caso aparece directaente la clase del objeto con sus 
  # respectivas dimensiones A tibble: 336,776 x 19
  # lo podríamos hacer para mtcars
  
  class(mtcars)
# al imprimir la clase de este objeto veremos que es un data frame [1] "data.frame"
#ahora si imprimieramos directamente mtcars solo veríamos la base de datos filas y columnas  
  mtcars
 
  # finalmente, otra forma de evaluar si estamos en presencia de una tibble es aplicar funciones que solo aplican para tibbles
#  Pista: imprime los objetos mtcars por un lado y nycflights13::flights que son respectivamente un data.frame y una tibble.

#Compara y contrasta las siguientes operaciones en el data frame y su equivalente en tibble. 

 df <- data.frame(abc = 1, xyz = "a")
 df$a

 
 df[,"xyz"]
df[,c("abc","xyz")]


ddt <- tibble(
  abc = 1,
  xyz = "a")

ddt$x
ddt$abc

ddt[,"xyz"]
ddt[,c("abc","xyz")]

# ¿En qué se parecen? 

# para ambos salidas la calidad de información es la misma, la columna de filas y columnas. Siempre y cuando
# definamos exactamente el nombre de columnas 
 
# ¿En qué difieren? 
 # el tibble es más restrctivo que el data.frame ya que cuando usamos "$" para selccionar una variable, en particualar, 
# si se es abc y llamamos solo a "a" en un data.frame seleccionaremos todas las columnas que comiencen con "a", si hay más de una 
# la búsqueda arrojará "NULL". Cuando hacemos una búsqueda en una tibble esta devuelve una tibble. No un vector como en el
#caso de las dfs
# en una tibble es necesario llamar con exactitud la variable, ya que si no la encuentra nos arrojará "NULL".
# tibblemejoró la forma de imprimir la información, esto es de gran ayuda cuando estamos trabajando con una gran cantidad
# de datos



# ¿Por qué a veces el data frame por defecto nos puede causar mucha frustración?
vignette("tibble")

# porque los códigos son más dificil de leer ya que hay que definir cada variable del dataframe.



  # Si tenemos el nombre de una variable almacenada en un objeto tipo string (por ejemplo myvar <- "mpg"),
#¿cómo podemos extraer la variable referenciada de una tibble? ¿Y en un data frame?

tibble <- as_tibble(mtcars)
myvar <- "mpg"

tibble[myvar]

# en un data.frame

mtcars[myvar]

# Toma la siguiente tibble formada por variables con nombres no sintácticos.

 df <- tibble(
  
  `1` = 1:12,
  `2` = `1` * 2 + `1`*runif(length(`1`))
)

#Extrae el valor de la variable `1`
 
 df[1] # como tibble
 df[[1]] # como vector 
 
#Haz un scatterplot de la variable `1`contra la variable `2`
 
 ggplot(df,aes(`1`,`2`)) +
   geom_point()
 
 
#Crea una nueva columna llamada `3`que sea el cociente de `2`entre `1`.
 
 
 df1 <- df %>% mutate(`3` = `2`/`1`)
 options(tibble.width = 100)
 options(dplyr.print_min = Inf)

#Renombra las columnas para que se llamen x, y, z respectivamente.
 
 rename(df1, x = `1`, y = `2`, z = `3`)
 transmute(df1, x = `1`, y = `2`, z = `3`) 
 ?rename
 
#¿Qué nombre crees que es mejor?
 
 # Las variabls x, y y z puede representar un concepto más general. Las variable por número pueden generar confusión
 # con el valor de las obs

 #  Investiga acerca de la función tibble:enframe() y tibble:deframe(). ¿Qué hace y para qué puede servirte?

 # enframe() converts named atomic vectors or lists to one- or two-column data frames.
 #For a list, the result will be a nested tibble with a column of type list. For unnamed vectors, the natural sequence is used as name column.
 
 # deframe() converts two-column data frames to a named vector or list, using the first column as name and the second column as value.
 #If the input has only one column, an unnamed vector is returned.
 
 enframe(1:3)
 enframe(c(a = 5, b = 7))
  deframe(enframe(3:1))
  deframe(tibble(a = 1:3))
 
#  ¿Cómo podemos controlar cuantos nombres de columna adicionales se imprimen en el footer de una tibble?
  
  
package?tibble
?print.tbl_df

 #podemos consultar el paquete de opciones con options(tibble.width = Inf)   # quiero ver todas las columnas de la tibblr





