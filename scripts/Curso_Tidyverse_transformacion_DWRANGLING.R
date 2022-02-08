#vamos a trabajar con el paquete dplyr

library(tidyverse)
library(nycflights13)


nycflights13::flights


?flights
View(flights)


#tibble, dataframe mejorado para mejorar el manejo por tidyverse

# int -> numeros enteros
# dbl ->  numeros reales
# chr -> vector de caracteres
# dttm ->  date + time 
# lgl -> logical, contiene valores booleanos True or False
# fctr ->  factor, variables categoricas
# date ->  fecha, día, mes, año

# filter ->  filtrar datos u observaciones a partir de valores concretos
# arrange ->  función para reordenar las filas 
# select  ->  seleccionar variable por su nombre
# mutate -> crea nuevas variables a partir de las existentes
# summarise -> colapsa varios valores para dar un resumen

# group_by -> opera la funcion a la que acompaña grupo o factor

#sesion 41


# FILTER

 jan1<-  filter(flights, month == 1, day == 1)


 apr1<-  filter(flights, month == 4, day == 27)

 
(dec25 <-  filter(flights, month == 12, day == 25))
 
# algunos operadores logicos
 
 sqrt(2)^2 == 2 # eso pasa porque el algoritmo realiza una aproximación
 near(sqrt(2)^2,2)
  1/pi *pi == 1  
 
  
#sesion 42 operadores booleanos
  
  
  
  filter(flights, month == 5 | month == 6)
 
  filter(flights, month == 5 | 6) # no es correcto
  
  
  #ahora solo quiero los vuelos que ocurrieren entre mayo y junio
  
  may_june <- filter(flights, month %in% c(5,6))

  
  #Ley Morgan
  
  # !(x&y) == !x|!y
  # !(x|y) == (!x)&(!y)
  
  
#ejemplo
  
  filter(flights, !(arr_delay > 60 | dep_delay > 60))
  
  
  filter(flights, arr_delay <= 60 , dep_delay <= 60)
  
  
  
  #sesion 42 valores NA
  
  
  
# NA : NOT AVAILABLE
  
  #is.na() podemos usarla
  #cuando filtramos debemos tener cuidado con esto
  
  #vamos a crear un pequeño dataframe
  
  df <- tibble(x = c(1,2,NA,4,5))
  
  df
  
  filter(df, x > 2)
  
  filter(df, is.na(x) | x > 2) 
  
  
  #sesion 44 funcion arrange
  
  #si tengo un df puedo buscar
  
  #las primeras 10 fils con
  
  
  head(flights, 10)
  
  #o las últimas, con
  
  tail(flights, 10)

  
  
  #reordenamos filas con ARRANGE
  
  sorted_date <- arrange(flights, year, month, day)
  
  head(sorted_date, 10)
  
  #o las últimas, con
  
  tail(sorted_date, 10)
  
  #si queremos ordenar por descendiente
  
  arrange(flights, desc(arr_delay))
  head(arrange(flights, desc(arr_delay)))
  
  head(arrange(flights, desc(dep_delay)))
  
  #
  
  arrange(df, x)
  arrange(df, desc(x))

  
  View(arrange(flights, carrier))  
  
  
  View(arrange(flights, desc(distance)))
  
  
  
  
  #ASSIGMENT 8
  
  ?flights
  #Questions for this assignment
  #Encuentra todos los vuelos que llegaron más de una hora tarde de lo previsto.
  
  filter(flights, (arr_delay > 60))
  
  #son 27789 vuelos
  
  #Encuentra todos los vuelos que volaron hacia San Francisco (aeropuertos SFO y OAK)
  
  filter(flights, dest == "SFO" | dest == "OAK")
  
  # 13643 vuelos con destino a San Francisco:aeropuertos SFO y OAK
  
  #Encuentra todos los vuelos operados por United American (UA) o por American Airlines (AA)
  
  filter(flights, carrier == "UA" | carrier == "AA" )
  
  # estos fueron 91394
  
  #Encuentra todos los vuelos que salieron los meses de primavera (Abril, Mayo y Junio)
  
  filter(flights, month %in% c(4,5,6))
  filter(flights, month == 4 | month == 5 | month == 6)
  
  #En total salieron 85369 vuelos
  
  #Encuentra todos los vuelos que llegaron más de una hora tarde pero salieron
  #con menos de una hora de retraso.
  
  filter(flights, arr_delay > 60, dep_delay < 60)
  filter(flights, arr_delay > 60 & dep_delay < 60) # alternativa
  
  
  # fueron 4956 vuelos
  
  #Encuentra todos los vuelos que salieron con más de una hora de retraso
  #pero consiguieron llegar con menos de 30 minutos de retraso (el avión aceleró en el aire)
  
  
  filter(flights, dep_delay > 60, arr_delay < 30)
  filter(flights, dep_delay > 60 & arr_delay < 30)
  
  # fueron 181 vuelos que cumplen con esta condición
  
  #Encuentra todos los vuelos que salen entre medianoche y las 7 de la mañana (vuelos nocturnos).
  
  
  View(flights)
  ?between
  ?flights
  
  View(filter(flights, hour <= 7))
  
  #son 50726 vuelos
  
  #Investiga el uso de la función between() de dplyr. ¿Qué hace? Puedes usarlo para resolver
  #la sintaxis necesaria para responder alguna de las preguntas anteriores?
  
  # la funcion permite separar un subconjunto de valores que se encuentran en un vector
  #por ejemplo, podemos utuilizarlo para extraer datos numéricos (el vector de datos debe ser  numerico)
  # es aplicable al inciso anterior,
  
  filter(flights, between(hour, 0,7))
  
  
    #¿Cuantos vuelos tienen un valor desconocido de dep_time?
    
  filter(flights, is.na(dep_time)) # 8255 vuelos
 
    
  #¿Qué variables del dataset contienen valores desconocidos?
  
  apply(is.na(flights), 2, sum)  
  
  ?apply
  # estas son dep_time, dep_delay, arr_time, arr_delay,
  #tailnum. air_time
  
  #¿Qué representan esas filas donde faltan los datos?
  
    #son observacuiones no disponibles.
  
  
    #Ahora vas a sorprenderte con la magia oscura... 
  #Contesta que dan las siguientes condiciones booleanas
  
  NA^0
  
  #da 1
  
  NA|TRUE
  
  #verdadero
  
  FALSE&NA
  TRUE&NA
  TRUE&FALSE
  FALSE&FALSE
  NA*0
  # False
  
  #Intenta establecer la regla general para saber cuando es o no es NA (cuidado con NA*0)
  
  
  
    #https://thomasadventure.blog/es/posts/r-contar-na/
  #http://betaeconomia.blogspot.com/2019/08/valores-perdidos-na-en-r-identificacion.html
  
  
  
  #sesion 45
  
  #usando la funcion SELECT
  
  arrange(flights, carrier)
  
  
  #quiero ciertos valores, por ejemplo una fila
  
  sorted_date[1,] # me entrega la fila 1 de sorted_date
  
  View(sorted_date[1024:1068,]) # filas 1 a la 3 de todas las variables del dataset
  
  #si quiero acotar el análisis a un conjunto de variables
  
  select(sorted_date[1024:1068,], dep_delay, arr_delay)
  
  
  # lo mismo para todo el data set
  
  select(flights, year, month, day)
  
  #otra forma de seleccionar usando ":" 
  
  select(flights, dep_time:arr_delay)
  
  #si quiero todas las columnas menos year, month, day
  
  select(flights, -(year:day))
  
  
  #si quiero encontrar a aquellas cuyas variables comienzan por un chracter
  
  select(flights, starts_with("dep"))

  #si quiero buscar todas las columnas que terminen con delay
  
  select(flights, ends_with("delay"))
  
  #si quiere algunas que contienen una "s"
  
  select(flights, contains("st"))
  
  # ahora las más utilizadas son aquellas expresiones regulares
  
  select(flights, matches("(.)\\1")) ## matches es del paquete dyplr, no confundir con match que es de stats

  # me busca variables seleccionadas cuando difieren en el subindice
  
  select(flights, num_range("x", 1:5)) # busca x1, x2, ..., x5
  
    
  
  #sesion 45
  
  # una variante de la select para renombrar es rename
  # se recomienda usar rename
  
  rename(flights, deptime = dep_time)

  #con select
  
  select(flights, deptime = dep_time) # solo me devuelve una columna
  
  
  # ahora, si quisiera seleccionar, pocas variables de interés
  # se eligen
  
  select(flights, time_hour, distance, air_time)
  
  #pero si me interesan ordenar las columnas en un orde, puedo añadir todas 
  #las demás columnas
  
  select(flights, time_hour, distance, air_time, everything())
  
  ## ASSIGNMENT 9
  
 # Questions for this assignment
#  Piensa cómo podrías usar la función arrange() 
  #para colocar todos los valores NA al inicio. 
  #Pisa: puedes la función is.na() en lugar de la función desc() 
  #como argumento de arrange.
  
  arrange(flights, desc(is.na(dep_delay)), dep_delay)

  
 # Ordena los vuelos de flights para encontrar los vuelos más retrasados en la salida.
  #¿Qué vuelos fueron los que salieron los primeros antes de lo previsto?

  arrange(flights, desc(dep_delay))
  
  ?flights
  
  
  
 #los dep_delay con números negativos son los que salieron con mucha anticipación,
  # si ordenamos de manera ascendente encontraremos esos vuelos
  #estos fueron los del:
  #7 de dicciembre del 2013, vuelo 97 N592JB, carrier B6
  
  anticipated_flights  <- arrange(flights, dep_delay)
  
  anticipated_flights[1:5,]
  #    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time arr_delay carrier flight tailnum
  #1  2013    12     7     2040           2123       -43       40           2352        48 B6          97 N592JB 
  #2  2013     2     3     2022           2055       -33     2240           2338       -58 DL        1715 N612DL 
  #3  2013    11    10     1408           1440       -32     1549           1559       -10 EV        5713 N825AS 
  #4  2013     1    11     1900           1930       -30     2233           2243       -10 DL        1435 N934DL 
  #5  2013     1    29     1703           1730       -27     1947           1957       -10 F9         837 N208FR 
  
  #  Ordena los vuelos de flights para encontrar los vuelos más rápidos. Usa el concepto de rapidez que consideres. 
  
  ?flights
  
  #Usaré la variable distancia y varriable tiempo en el aire para y 
  #las ordenaré de mayor a menor
  
 (arrange(flights, desc(distance/air_time)))
  
  
  #¿Qué vuelos tienen los trayectos más largos?
  
  View(arrange(flights, desc(distance)))
  # el vuelo más largo es entre JFK y HNL con una distancia de 4983 millas. New York a Honolulu
  
  #Busca en Wikipedia qué dos aeropuertos del dataset alojan los vuelos más largos. 
  
  #¿Qué vuelos tienen los trayectos más cortos?
  
  View(arrange(flights, distance))
  # los más cortos son entre EWR-LGA y EWR-PHL
  

  #Dale al coco para pensar cuantas más maneras posibles de seleccionar los campos dep_time,
  #dep_delay, arr_time y arr_delay del dataset de flights. 
  
  #EJEMPLOS :
   
ejemplos_campos_1 <-  select(flights, dep_time, dep_delay, arr_time, arr_delay)
ejemplos_campos_2 <- select(flights, dep_time:arr_delay, -(sched_dep_time),- (sched_arr_time))  

  #¿Qué ocurre si pones el nombre de una misma variable varias veces en una select()?
    
select(flights, dep_time, dep_time, dep_time)
      # no pasa nada, solo sobreescribe

   # Investiga el uso de la función one_of() de dplyr.

?one_of # selecciona las variables pasadas de un vector

  #Investiga cómo puede ser útil la función one_of() 
#de la pregunta anterior en conjunción con el vector de variables 
    #c("year", "month", "day", "dep_delay", "arr_delay")
  
# primero, debemos crear el vector

variables_flights <- c("year", "month", "day", "dep_delay", "arr_delay")

#luego, llamamos a este vector 

select(flights, one_of(variables_flights))



  #Intenta averiguar el resultado del siguiente código. Luego ejecútalo y 
#a ver si el resultado te sorprende.
  
  select(flights, contains("time"))
  
  # selecciona las variables que tiene el string time
    #Intenta averiguar cómo lo hacen las funciones de ayuda de la select para tratar 
#el caso por defecto y cómo lo puedes cambiar.
   
  #el caso por defecto es siempre verdadero
  ?select
# lo podemos cambiar con 
 # contains(match, ignore.case = TRUE, vars = NULL)
  
  #select(flights, contains("time", ignore.case = TRUE/FALSE, vars = NULL))
  
  ##sesion 47
  
  ##mutate creando nuevas variables con mi data set
  
  flights_new <- select(flights, year:day, 
                        ends_with("delay"),
                        distance,
                        air_time)

  
  View(flights_new)  

  mutate(flights_new, time_gain = arr_delay - dep_delay,
         flight_speed = distance/(air_time/60))
  
  #creando nuevas variables con mi data set, quedándme solo con las nuevas uso transmute()
  
  transmute(flights_new, time_gain = arr_delay - dep_delay,
              flight_speed = distance/(air_time/60) )
  
  ## mutate y transmute, l funcion debe ser vectorizada
  #podemos hacer operaciones aritmeticas
  #reglas del reciclaje
  #funciones agregadas: x/sum(x ) coo una proporcion del total
  # o x - mean(x), (x-mean(x))/sd(x) tipificacion
  #x - min(x)/x- max(x)
  #aritmetica modeular %/% cociente de la división entera, %% resto de la division entera
   #ejemplo

  transmute(flights, 
            air_time,
            air_hour = air_time %/% 60, # parte entera de la duivision
            air_minute = air_time %% 60) #resto de los minutos de las division
  
  
  # logaritmos, log = logaritmos neperiano, log2(), log10(), 
  #offset: lead(), lag()
  
  df <-  1:12
lag(1:12)   #movemos posiciones
lead(1:12)

#funciones acumulativas cumsum(), cumprod(), cummin(), cummax(), cummin()

cumsum(df)
cumprod(df) # sucesion de numeros factorials
cummin(df) # acumula el numero mas pequeños
cummax(df) # acumula el numero más pequeño
cummean(df) # promedios de los acumulados

#comparaciones logicas : >, >=, <, <=, ==, !=

transmute(flights, has_been_deleyed = dep_delay > 0) # operaciones booleanas

#rankings: ordenar min_rank()

df <- c(1, 2, 3, NA, 8, 1, NA, 9, 7, 6)
min_rank(df)

min_rank(desc(df))


dense_rank(df)
# variantes row_number()
#dens_rank()
#percent_rank()  porcentaje respecto al total
#cume_dist() percentiles de cada uno de los eleemntos
#ntile(df, n = ?)

ntile(df, n = 4)

    transmute(flights,
              dep_delay,
              ntile(dep_delay, n = 100))

    
    
    #ASSIGNMENT 9
    #El dataset de vuelos tiene dos variables, dep_time y sched_dep_time muy útiles 
    #pero difíciles de usar por cómo vienen dadas al no ser variables contínuas.
    #Fíjate que cuando pone 559, se refiere a que el vuelo salió a las 5:59... 
    #Convierte este dato en otro más útil que represente el número de minutos que horas desde media noche. 
    
    
    
  select(flights, dep_time, sched_dep_time)
  
  View(flights)
  
  
  transmute(flights, dep_time, 
                    dep_time_hour = dep_time%/% 100,
                    dep_time_minute = dep_time %% 100,
                    dep_time_total_minutes = dep_time_hour * 60 + dep_time_minute)
    
    
    
View(flights)

#Compara las variables air_time contra arr_time - dep_time. 
    

transmute(flights, air_time, 
          arr_time,
          dep_time,
          contra_time = arr_time - dep_time)
                
      
    #¿Qué esperas ver?
    #esperamos ver que exista una relacion entre los horarios de llegado y salida 
    # con los tiempos en el aire

  #  ¿Qué ves realmente?

    #vemos diferencias entre el tiempo en el aire y contra_time
     
      # ¿Se te ocurre algo para mejorarlo y corregirlo?

  # podríamos estandarizar todos los variables a un término de tiempo comun, por ejemplo tiempo en minutos
  #y ver las diferencias entre salidas y llegadas, ver si esas diferencias se debe al huso horario
    

    #Compara los valores de dep_time, sched_dep_time y dep_delay.

          select(flights, dep_time, sched_dep_time, dep_delay)

    #Cómo deberían relacionarse estos tres números? Compruébalo y haz las correcciones numéricas que necesitas.
          
          
          min(flights$dep_delay, na.rm = T)
          
          arrange(flights, desc(dep_delay))
   
    #En particular, los valores  de dep_delay están en minutos, por lo tanto, podemos dejar
          #todos los valores expresados en minutos, ya que dep_time y sched_de_time estan en horaminutos. Dememos normalizar
          # en este caso dep_time - sched_dep_time = dep_delay, pero las unidades son difrentes. Debemos ver los valores
          #de atrasos de las salidas para ver como proceder
          
          min(flights$dep_delay, na.rm = T) # valor mínimo
          max(flights$dep_delay, na.rm = T) #alor máximo para dep_delay
          
          
          #voy a crear una variable nueva para dep_delay con el tiempo estandarizado en horas, por tanto lo dividiré
          #en 60 min, es decir,  1 hr/60 min = 1
          
          #esto es,
          
          
          #ahora voy a tomar los minutos y convertirlos a horas para dep_time y sched_dep_time
                    
          data <- transmute(flights,
                                  dep_time,
                                  sched_dep_time,
                                  dep_time_min = ((dep_time %/% 100) * 60 + dep_time %% 100) %% 1440,
                                  sched_dep_time_min = ((sched_dep_time %/% 100) * 60 + sched_dep_time %% 100) %% 1440,
                                  diference = dep_delay + sched_dep_time_min - dep_time_min)
          
          
          
           ggplot(data = data) +
           geom_point(mapping = aes(diference, sched_dep_time_min))
   
               
    #Usa una de las funciones de ranking para quedarte con los 10 vuelos más retrasados de todos. 
            
            #ordenamos por dep_delay dentro del arrabge para obtener los más retrasados
            
           View (arrange(flights, desc(dep_delay))[1:10,])
    
    #Aunque la ejecución te de una advertencia, qué resultado te da la operación
    
    1:6 + 1:20
    
    #el resultado es,
    
   #   [1]  2  4  6  8 10 12  8 10 12 14
    # [11] 16 18 14 16 18 20 22 24 20 22
    
    # el warning es
    
    #Warning message:
    #  In 1:6 + 1:20 :
    #  longitud de objeto mayor no es múltiplo de la longitud de uno menor
    
    #Además de todas las funciones que hemos dicho, las trigonométricas también son funciones vectoriales 
    #que podemos usar para hacer transformaciones con mutate. 
    #Investiga cuales trae R y cual es la sintaxis de cada una de ellas.
    
    #algunas de las funciones
    ?cos(x)
    sin(x)
    tan(x)
    acos(x)
    asin(x)
    atan(x)
    atan2(y, x)
    
    #ejemplos
    x <- seq(-3, 7, by = 1/8)
    tx <- cbind(x, cos(pi*x), cospi(x), sin(pi*x), sinpi(x),
                tan(pi*x), tanpi(x), deparse.level=2)
    op <- options(digits = 4, width = 90) # for nice formatting
    head(tx)
    tx[ (x %% 1) %in% c(0, 0.5) ,]
    options(op)
    
    
    
    # sesion 49 SUMMARISE()
    
    summarise(flights, delay = mean(dep_delay, na.rm = T))

    by_month_group <- group_by(flights, year, month)    
    
    #por dia
    
    summarise(by_month_group, delay = mean(dep_delay, na.rm = T))
    
    
    by_day_group <-  group_by(flights, year, month, day)

    summarise(by_day_group, delay = mean(dep_delay, na.rm = T), 
          median = median(dep_delay, na.rm = T),
          )    

    mutate(summarise(group_by(flights, carrier), 
             delay = median(dep_delay, na.rm = T)),  
             sorted = min_rank(delay))
   
    transmute(summarise(group_by(flights, carrier), 
                     delay = median(dep_delay, na.rm = T)),  
           sorted = min_rank(delay))
    
    # SESION 50 USANDO PIPES
    
    
    group_by_dest <- group_by(flights, dest)
    
    
    delay <-  summarise(group_by_dest, 
                        count = n(),
                        dist = mean(distance, na.rm =T),
                        delay = mean(arr_delay, na.rm = T)           
                        )
    
    
    deay_new <- filter(delay, 
             count > 100, 
               dest != "HNL")
    
    
    ggplot(data = deay_new, mapping = aes(x = dist, y = delay)) +
      geom_point(aes(size = count), alpha = 0.35) + 
      geom_smooth(se = F) +
      geom_text(aes(label = dest), alpha = 0.30)
      
    
    ?geom_smooth
    
    
    
    delay <- flights %>% 
                  group_by(dest) %>% 
                      summarise(
                                  count = n(),
                                  dist = mean(distance, na.rm = T),
                                  delay = mean(arr_delay, na.rm = T)) %>% 
                       filter(count > 100, dest != "HNL")
    
    # cuando tienes x %>% f(y) <-> f(x,y)
    # x %>% f(y) %>% g(z) <-> g(f(x,y), z)...
    #ggplot no permite pipes (esta es más antigua)
    #pipes es de mgritte es mas nueva
    #ggvis permite pipes 
    
    
  #SESION 51 : eliminando NA´s
    
    flights %>% 
      group_by(year, month, day) %>%
        summarise(mean = mean(dep_delay, na.rm = T),
                  median = median(dep_delay, na.rm = T),
                  sd = sd(dep_delay, na.rm = T),
                  count = n())
    
    not_cancel <- flights %>% 
                    filter(!is.na(dep_delay),!is.na(arr_delay))
      
    
    
    not_cancel %>%
      group_by(year, month, day) %>%
      summarise(mean = mean(dep_delay, na.rm = T),
                median = median(dep_delay, na.rm = T),
                sd = sd(dep_delay, na.rm = T),
                count = n())
    
    # SESION 52 CONTAR Y VISUALIZAR RESUMENES CORRECTAMENTE
    
    # usar conteo, valores que no son desconocidos
    
    delay_numtail <- not_cancel %>% 
                 group_by(tailnum) %>% 
                      summarise(delay = mean(arr_delay))
    
    
    #graficamos los datos
    
    
    ggplot(data = delay_numtail, mapping = aes(delay)) +
      geom_freqpoly(bindwith = 5) #cada 5 valores hay un punto del polígono
    
    
    ggplot(data = delay_numtail, mapping = aes(delay)) +
      geom_histogram(bindwith = 5) 
    
    
    delay_numtail <- not_cancel %>% 
                        group_by(tailnum) %>% 
                          summarise(delay = mean(arr_delay),
                                    count = n())
      
    
    ggplot(data = delay_numtail, mapping = aes(x = count, y = delay)) +
      geom_point(alpha = 0.2) 
    
    #para no considerar los vuelos que han salido poco
    
    delay_numtail %>% 
      filter(count > 50) %>%
            ggplot(mapping = aes(x = count, y = delay)) +
                geom_point(alpha = 0.2) # dispersión mucho más concentrada
      
   #sesion 53~: ejemplo beisbol
    
    
    install.packages("Lahman")
    
    View(Lahman::Batting)
    # AB numero de veces que tuvo la oportuidad de batear
    # HA numero de veces que alcanzo la base por pegarle
    
    ?Lahman
    
    batting <- as_tibble(Lahman::Batting)
    
    
  batters <- batting %>%
    group_by(playerID) %>% 
    summarise(hits = sum(H, na.rm = T),
              bats = sum(AB, na.rm = T),
              average.bats = hits/bats)
  
  batters %>% filter(bats > 100) %>% 
    ggplot(mapping = aes(x = bats, y = average.bats)) +
    geom_point(alpha = 0.2) + 
    geom_smooth(se = F)

  batters %>% filter(bats > 100) %>%
    arrange(desc(average.bats))    
    
  #VER PELI DE BRAD PITT
  #SESION 55: Funciones estadísticas útiles que se pueden combinar con summary
  
  #medidas de centralización 
  
  not_cancel %>% 
    group_by(carrier) %>% 
      summarise(mean = mean(arr_delay),
                mean2 = mean(arr_delay[arr_delay > 0]),
                median = median(arr_delay))
  
  #medidas de dispersión
  #sd 
  #rango intercuartílico distancia ente el percentil 25 y 75
  
   not_cancel %>% 
     group_by(carrier) %>% 
     summarise(
            sd = sd(arr_delay),
            iqr = IQR(arr_delay),
            mad = mad(arr_delay)
     ) %>% 
              arrange(desc(sd))
  
  ?mad
  
   #medidas de orden
   not_cancel %>% 
     group_by(carrier) %>% 
     summarise(
       first = min(arr_delay),
       q1 = quantile(arr_delay, 0.25),
       median = quantile(arr_delay, 0.5),
       q3 = quantile(arr_delay, 0.75),
       last = max(arr_delay)
     )
 
   #medidas de posición
   not_cancel %>% 
     group_by(carrier) %>% 
     summarise(
       first_dep = first(dep_time),
       second_dep = nth(dep_time, 2),
       third_dep = nth(dep_time, 3),
       last_dep = last(dep_time)
     )
  
    
   not_cancel %>%
     group_by(carrier) %>% 
     mutate(rank = min_rank((dep_time))) -> temp1
   
   
   not_cancel %>%
     group_by(carrier) %>% 
     mutate(rank = min_rank((dep_time))) %>% 
     filter(rank %in% range(rank)) -> temp
   
   ?min_rank
x <- c(1,5,7,3,2)   
range(x)

# funciones de conteo


flights %>% 
  group_by(dest) %>% 
        summarise(
          count = n(),
          carriers = n_distinct(carrier),
          arrivals = sum(!is.na(arr_delay))
        ) %>%  arrange(desc(carriers))
?n_distinct

x <- sample(1:10, 1e5, rep = TRUE)
length(unique(x))
n_distinct(x)

not_cancel %>% 
   count(tailnum, wt = distance) # aca estamos sumando las millas por vuelo

## sum/mean de valores lógicos

not_cancel %>% 
  group_by(year,month,day) %>% 
  summarise(n_prior_5 = sum(dep_time < 500))

not_cancel %>% 
  group_by(year, month, day) %>% 
  summarise(more_than_hour_delay = mean(arr_delay > 60))

not_cancel %>% 
  group_by(carrier) %>% 
  summarise(more_than_hour_delay = mean(arr_delay > 60)) %>% 
  arrange(desc(more_than_hour_delay))
?mad


# Sesión 55: agrupaciones y múltiples desagrupaciones


  daily <- group_by(flights, year, month, day)  # acá estamos eliminando niveles, cuidado con promedios
  (per_day <- summarise(daily, n_fl = n()))
  (per_month <- summarise(per_day, n_fl = sum(n_fl)))
  (per_year <- summarise(per_month, n_fl = sum(n_fl)))
  
  
  business <- group_by(flights, carrier, dest, origin)
  
  summarise(business, n_fl = n()) %>% 
        summarise(n_fl = sum(n_fl)) %>% summarise(n_fl = sum(n_fl))
  
  
 # si quiero desagrupar
  
  daily %>% 
     ungroup() %>% 
          summarise(n_fl = n())
  daily %>% 
    summarise(n_fl = n())
  
  
  
  
  business %>% 
    ungroup() %>% 
    summarise(n_fl = n())
  
  #SESION 55 MUTATES Y FILTROS POR SEGMENTO
  
  
  temp <- flights %>% 
    group_by(year, month, day) %>% 
    filter(rank(desc(arr_delay)) <= 10)
  
  x <- c(1, 3, 6, 8, 3, 2, 4, 9)

 filter( rank(desc(x)) > 4)
  
  ?rank    
  
 
 max(flights$arr_delay, na.rm = T)
 
 
 popular_dest <- flights %>% 
                group_by(dest) %>% 
                            filter(n() > 365 )
 
 
 View(popular_dest)
 
 
 popular_dest %>%  
   filter(arr_delay > 0) %>%
        mutate(prop_delay = arr_delay/sum(arr_delay)) %>% 
                    select(year:day, dest, arr_delay, prop_delay)
 
 # SESION 56
 #Analisis exploratorio de datos
 # Modelar
 #Representación Gráfica
 #Transformación de datos
 
 # ¿ qué tipon de variacion sufren las variables?
 # ¿ Que tipo de cavariación sufren las variables?
 
 # * Variables : catntidad, valor opropiedad medible
 # * Valor : estado de una variable al ser medida
 # * observación : conjunto de medida tomada en condiciones similares y simultçaneas
 #  data point, conjunto de variables tomados para cada variable
 
 # * datos tabulares : conjunto de valores, asociado a cada variabley observación
 # si los datos están limpios, cada valor tiene su propia celda, cada variable
 # tiene su columna, y cada observación su fila. Cada variable tiene  su columna
 # y cada cobservación tiene su fila.
 
 ## Variación
 
 # * es la tendencia que tienen los valores de una variable a cambiar
 # cuando se mide más de una vez.
 #Cuando medimos, existencia de un error instrumental
 
 # variables categóricas :  pueden tomar pocos valores. Factoro vector 
 #de caracteres
 
 diamonds
 
 # para su representación gráfica podemos usar un grafico de barras
 
 ggplot(data = diamonds) +
   geom_bar(mapping = aes(x = cut))
 
#representación numérica en una tibble
  diamonds %>% 
   count(cut)
 
 # Variable continua: conjunto infinito de valores ordenaados (número, fechas)
  
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), bindwith = 0.5)
# ojo que en un histograma los valores están juntos en uno de barras separadas

#la representación numérica en este caso

diamonds %>%
  count(cut_width(carat, 0.5)) # toma la variable continua y la agrupa en rangos
                              # generando un tibble

diamonds %>%           #esta no ayuda mucho
  count(carat)

diamonds_filter <- diamonds %>%
  filter(carat < 3)

ggplot(data = diamonds_filter) + 
  geom_histogram(mapping = aes(x = carat), binwidth = 0.005)

ggplot( data = diamonds_filter, mapping = aes(x =carat, color = cut)) +
      geom_freqpoly(binwidth = 0.05)  # cuanto queremos este tipo de graficas, es mejor
                                    # usar geom freq poligon, usa líneas en vez de barras 

# SESIÓN 61
# * Cuáles son los va,lores más comunes?
# * cuáles son los valores más raros? por qué? cumple con lo que esperábamos?
# * vemos algún patron inusual?
# * Qué tipo de info puede entregar un grafico


#SESION 62

# * Si hay obs que son similares puede que existan agrupaciones.
# * Qué determina que los elementos de un cluster sean similares entre sí.
# * Qué determina que clusters separados sean diferentes entre sí.
# * describiry explicar cada uno de los cluster
# * Por qué alguna observación puede ser clasificada en el cluster erróneo

faithful
?faithful

ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_histogram(binwidth = 0.2)

# SESION 63:  Outliers de inforrmación

# outliers: observación inusual, no encajan en un ningún grupo
#           nuevo descubriemiento.

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) + coord_cartesian(ylim = c(0,100))

# cuando tenemos este tipo de graficos y no logramos ver outliers podemos hacer un cambio de coordenadas

unusual_diamonds <- diamonds %>% 
                            filter( y < 2 | y > 30) %>% 
                            select(price, x,y,z) %>% 
                            arrange(y)
View(unusual_diamonds) # diferentes tipos de outliers pueden deberse a errores de digitación



#SESION 64
# cuando queremos eliminar valores que se encuentran

good_diamonds <-diamonds %>% 
              filter(between(y, 2.5,29.5))

good_diamonds <- diamonds %>% 
   mutate(y = ifelse( y < 2 | y > 30, NA, y))


ggplot(good_diamonds, mapping = aes(x = x, y = y)) +
    geom_point()
#cómo eliminamos el warnings,añadiedno na.rm = T

ggplot(good_diamonds, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = T)


nycflights13::flights

flights %>% mutate(cancelled = is.na(dep_time),
                   sched_hour = sched_dep_time %/% 100,
                   sched_min = sched_dep_time %% 100,
                   sched_dep_time = sched_hour + sched_min/60) %>% 
                  ggplot(mapping = aes(sched_dep_time)) +
                      geom_freqpoly(mapping = aes(color = cancelled), binwidth = 1/4)



# Questions for this assignment 12

 # 1. Explora la distribución de las variables x, y, z del dataset de diamonds. ¿Qué podemos inferir?
?diamonds
 #analizando las distribuciones con ggplot
 #distribución para x correspondiente a la longitud del diamante. 

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = x), binwidth = 0.01)

ggplot(data = diamonds) +
  geom_freqpoly(mapping = aes(x = x, color = cut), binwidth = 0.01)

# Lo longitud de los diamantes se encuentra entre 3 y bajo nueve, con algunas valores ailados
# posiblemente errores de medida. Hay cerca de siete picos en la distribución en 4-4.5, 5, 5.5, 7, 7.5 y 8 mm.
# concentrandose en la mayor parte en aquellos que miden 4-4.5 mm. Al patrecer es más 
# dificultoso conseguir diamantes más largos.

#distribucióm para y correspondiente a la anchura.

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.01) +
      coord_cartesian(xlim = c(0,15))

# en este caso la anchura de los diamantes se encuentra entre 0-10 mm. Los que se alejan de este rango,
# son poco frecuentes en la naturaleza, al parecer. los anhos más comunes son los que tienen una medida de 
# 4.5, 5.5, 6, 6,6-7 y 8mm. Mientras más grandes más dificil encontrar diamantes con mayor dificultad de encontrarlos.
# Sin embargo, los diamantes más comunes deben enocntrarse entre 5 y 6 mm de ancho.

# respecto a la profundida z

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = z), binwidth = 0.5) +
  coord_cartesian(xlim = c(0,10))
  
 #la distribución de la profundidad del diamante se encuentra entre 2.5 y 6.25 mm. Los más comunes entre 2.5 y 4 mm.

# finalmente, las dimensiones más comunes es probable que se encuentre entre 4mm *5.5mm* 6mm. Siendo más dificil encontrar
# diamantes de dimensiones más grandes y es probable que estas correspondan a eventos geológicos determinados por zonas de extracción
 
 # Busca un diamante (por internet por ejemplo) y decide qué dimensiones pueden ser aceptables 
#para las medidas de longitud, altura y anchura de un diamante.

#dimensiones de un diamante no son tan primordiales

# If you’re shopping for the perfect diamond, think beyond the carats. A diamond’s measurement is more than just carats–consider
# its shape, cut, and length-to-width ratio as well. 
# With a little research, it’s easy to find a diamond where all of these factors work together to maximize the diamond’s beauty. 
# A diamond’s size is described in millimeters (mm). These measurements are roughly comparable to carats. A 1-carat round diamond 
# is typically 6.5 mm, while a 1.25-carat round diamond is 6.8 mm.
# For square-cut diamonds like princess, cushion, and asscher, 1-carat is typically 5.5 mm and a 1.25 carat is 6 mm.  
# cuando hablamos del tamaño, al parecer, se refere a la longitud.


# 2. Explora la distribución del precio (price) del dataset de diamonds. ¿Hay algo que te llame la atención o 
# resulte un poco extraño?
# Recuerda hacer uso del parámetro binwidth para probar un rango dispar de valores hasta ver algo que te llame la atención.

  ggplot(data = diamonds) +
      geom_histogram(mapping = aes(x = price), binwidth = 1) + 
    coord_flip()

  #hay una distribución asimétrica positiva concentrandose en precios bajo 1500
  # observamos casi constante de frecuancias de precios sobre los 5000 hasta su max.
  # pero por  otro lado, vemos que puede existir subgrupos dentro de la distribución. Y un comportamiento 
  # entorno a cada precio ya que se observan picos.
  # el valor de los diamantes no aumenra linealmente con la dimensión
  
  # ¿Cuantos diamantes hay de 0.99 kilates? ¿Y de exactamente 1 kilate?

      # * vamos a contar haciendo un count()
  
    diamonds %>% filter(carat == 0.99) %>%
            count()
  View(diamonds)
  
  # *  tenemos diamantes 23 diamantes de 0.99 kilates
  
    diamonds %>% filter(carat == 1.00) %>%
    count()
  View(diamonds)
  
  # *  tenemos diamantes 1558 diamantes de 1.00 kilate
  
    # ¿A qué puede ser debida esta diferencia?

  # esto puede deberse a que difícilmente son fabricados los diamantes de 0.99 kilates, ya que su producción depende de la demanda, 
  #que no es lineal. Puedeb existir diferencias grandes entre 0.99 kilates y 1.01
  
  
  # 4. Compara y contrasta el uso de las funciones coord_cartesian() frente xlim() y ylim() para hacer zoom en un histograma.

   # x lim y ylim es un atajo para modificar escalas individuales de un eje, cualquier valor
  #por fuera de este limite se reemplaza con NAs.  Este removerá data lo que puede llevar
  # a resultados involuntarios.  Ahor, si queremos cambiar los limites de los ejes x e y sin 
  # eliminar ningún valor, es recomendable utilizar coord_cartesian()
  
  
 # ¿Qué ocurre si dejamos el parámetro binwidth sin configurar?
  
  ?geom_histogram
  
  ggplot(data = diamonds) +
    geom_histogram(mapping = aes(x = price), binwidth = 30)
  
  
  ggplot(data = diamonds) +
    geom_histogram(mapping = aes(x = price))
  
  # en este caso el valor por default que coupa cuando no defiimos el binwidth
  # utiliza el stat_bin() o bins = 30, cantidad de barritas, contenedores.
  
 # ¿Qué ocurre si hacemos zoom y solamente se ve media barra?
  
 ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price)) +
   coord_cartesian(ylim = c(0,10000))
 
 # podemos ajustar eñ ylim en coord_cartesian
 
# 5. ¿Qué ocurre cuando hay NAs en un histograma? 
 
 good_diamonds <- diamonds %>% 
   mutate(y = ifelse( y < 2 | y > 30, NA, y))
 
 
 ggplot(data = good_diamonds) +
   geom_histogram(mapping = aes(x = y), binwidth = 0.01)
 
 # geom_histogram remueve aquellas filas con NAs
 
 ggplot(data = good_diamonds) +
    geom_histogram(mapping = aes(x = y))
 
 
 
# ¿Qué ocurre cuando hay NAs en un diagrama de barras?
 
 ggplot(data = good_diamonds) +
   geom_bar(mapping = aes(x = y))
 
 
 ?geom_bar
 # geom_bar elimina aquellas filas con va,ores no finitos
 
# ¿Qué diferencias observas?
 
 # geom_bar utiliza stat_count y geom_histogram utiliza stat_bins. geom_bar utiliza un binwidth
 # menor, por tanto la cantidad de barras es mayor que el histograma
  
# 6. ¿Qué hace la opción na.rm = TRUE en las funciones mean() y sum()?

 ?na.rm
 
 # si el valor corresponde a verdadero los valores perdidos son removidos y permiten tanto, la suma como el promedio
 
 ?mean
 
 
 
 # SESION 65 covariación
 
 # tendencia de dos o más variables varíen de forma conjunta, relación entre dos o más variables
 # categoría vs  variable contínua
 
 ggplot(diamonds) + 
   geom_freqpoly(mapping = aes(x = price, color = cut), binwidth = 500) # discrepncias en distribución
 
 ggplot(diamonds) +
   geom_bar(mapping = aes(x = cut))
 
 ggplot(diamonds, mapping = aes( x =price, y = ..density..)) +
   geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
 
 # SESION 66 BOXPLOT
 
 ggplot(diamonds, mapping = aes( x = cut, y = price)) +
   geom_boxplot()
   
 # ojo que el corte es factor ordenado. si existe este orden el boxplor
 #lo hará
 
 
 ggplot(data = mpg, mapping = aes(x = class, y= hwy)) +
   geom_boxplot()
 
 #reordenado
 
 ggplot(data = mpg) +
   geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy ))
 
 ggplot(data = mpg) +
   geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy )) + 
   coord_flip()
 
 #La covariación de factores a través de heatmaps
 # categoría vs categoría
 
  ggplot(data = diamonds) +
   geom_count(mapping = aes(x = cut, y = color))
 
  #conteo de variables cruzadas
  
 diamonds %>% 
   count(color, cut)
 
 # mapa de colores
 diamonds %>% 
    count(color, cut) %>% 
   ggplot(mapping = aes(x = cut, y = color)) + 
   geom_tile(mapping = aes(fill = n))
 
 # otros pauquetes para este tipo de graficas
      # d3heatmap
      # heatmaply
 
 
 # SESION 68 COVARIACIÓN DE VARIABLES CONTINUAS
 
    ggplot(data = diamonds) +
      geom_point(mapping = aes(x = carat, y = price), alpha = 0.01 ) # puede ser muy pesado hacer esto
    
  # geom_bins  
 # geom_hex
    install.packages("hexbin")
    library(hexbin)
ggplot(data = diamonds) +
  geom_bin2d(mapping = aes(x =carat, y = price))





ggplot(data = diamonds) +
  geom_hex(mapping = aes(x =carat, y = price))


# Si queremos tratar una v. continua como una categoria 

diamonds %>% 
  filter(carat < 3) %>% 
ggplot(mapping = aes(carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)), varwidth = T)


diamonds %>% 
  filter(carat < 3) %>% 
  ggplot(mapping = aes(carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)), varwidth = T)



#SESION 69 Visualización de patrones


# la relación entre variables que encontramos se deben al azar
# relaciones que implican dicho patron, qu{e forma tiene-lineal, log, expo
# fuerza entra la relación entre las variables
# ¿ otras variables afectadas?
# ¿esta relación cambia con subgrupos?

ggplot(data = faithful) +
  geom_point(mapping = aes(eruptions, y = waiting))


library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

mod


diamonds_pred <- diamonds %>% 
                  add_residuals(mod) %>% 
                    mutate(res = exp(resid))
        


View(diamonds_pred)

ggplot(data = diamonds_pred) +
  geom_point(mapping = aes(x = carat, y = resid))


ggplot(data = diamonds_pred) +
  geom_boxplot(mapping = aes(x = cut, y = resid))



#Questions for this assignment
#Es hora de aplicar todo lo que hemos aprendido para visualizar mejor los tiempos de salida para vuelos cancelados vs los no cancelados.
#Recuerda bien qué tipo de dato tenemos en cada caso. ¿Qué deduces acerca de los retrasos según la hora del día a la que está programada el vuelo de salida?


  
#  ¿Qué variable del dataset de diamantes crees que es la más importante para poder predecir el precio de un diamante?
#  ¿Qué variable del dataset de diamantes crees que es la que más correlacionada está con cut?
#  ¿Por qué combinar estas dos variables nos lleva a que los diamantes con peor calidad son los mas caros?
# Instala el paquete de ggstance y úsalo para crear un boxplot horizontal. Compara el resultado con usar el coord_flip() que hemos visto en clase.

# Los boxplots nacen en una época donde los datasets eran mucho más pequeños y la palabra big data no era más que un concepto futurista. De ahí que los datos considerados con outliers tuvieran sentido que fueran representados con puntos dado que su existencia era más bien escasa o nula. Para solucionar este problema, existe el letter value plot del paquete lvplot. Instala dicho paquete y usa la geometría geom_lv() para mostrar la distribución de precio vs cut de los diamantes. ¿Qué observas y qué puedes interpretar a raíz de dicho gráfico?
  
#  Compara el uso de la geometría geom_violin() con un facet de geom_histogram() y contra un geom_freqpoly() coloreado. Investiga cuales son los pros y los contras de cada uno de los tipos de representación.

# Si tenemos datasets pequeños, a veces es útil usar la opción que ya conocemos de geom_jitter() para ver la relación entre una variable contínua y una variable categórica. El paquete de R ggbeeswarm tiene un par de métodos similares a geom_jitter() que te pueden ayudar a tal efecto. Listalos y haz un gráfico con cada uno de ellos para ver qué descripción de los datos podemos extraer de cada uno. ¿A qué gráfico de los que ya has visto durante esta práctica se parece?
  
#  Los mapas de calor que hemos visto tienen un claro problema de elección de los colores. 

# ¿Cómo podríamos reescalar el campo count dataset de diamantes cuando cruzamos color y cut para observar mejor la distribución de dicho cruce?
#  ¿Por qué resulta mejor usar la estética aes(x = color, y = cut) en lugar de aes(x=cut, y = color)?
#  Utiliza la geom_tile() junto con dplyr para explorar si el promedio del retraso de los vuelos varía con respecto al destino y mes del año. 
#¿Qué hace que este gráfico sea dificil de leer o de interpretar?
 # ¿Cómo puedes mejorar la visualización?
  #En lugar de hacer un resumen de la distribución condicional de dos variables numéricas con un boxplot, se puede usar un polígono de frecuencias. 

#¿Qué hay que tener en cuenta cuando usas cut_width() o cuando usas cut_number()?
#  ¿Cómo influye este hecho en la visualización 2D de carat y price
#Da la mejor visualización posible de carat dividido por price.
#Compara la distribución del precio de los diamantes grandes vs diamantes pequeños. Elige el concepto de grande y pequeño que consideres. Comenta el resultado.

# Combina diferentes técnicas de ggplot para visulaizar la distribución combinada de cut, carat y precio.

# Los plots en 2D pueden revelar outliers que no se ven en plots de una sola dimensión. Por ejemplo, algunos puntos del plot dado por

ggplot(data = diamonds) + 
  
  geom_point(mapping = aes(x = x, y = y)) + 
  
  coord_cartesian(xlim = c(4,12), ylim = c(4,12))

# hacen destacar muchísimo los outliers combinando x con y, a pesar de que por separado parecen valores normales. 

# Intenta averiguar porqué un scatterplot resulta más efectivo en este caso que un gráfico con agrupaciones.




# SESION DATA WRANGLING 
# Limpieza de datos
# transformación dplyr
# técnicas para datos específicos





