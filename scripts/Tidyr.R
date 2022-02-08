# Sesión 89: Limpieza de datos

    # Qué consideramos como un dataset "limpio"
      # * para transformaciones y visualizaciones
        # * tres cosas, cada variable deb tener su propia columna. no deben haber mezclas
        # * cadas observaciones debe teener su propia fila
        # * cada valor debe estar en su propia celda, debe estar row
        
      # consistencia en el alamacenamiento de datos, usamos una rutina para trrabajar de forma cómoda
        # los paquetes están pensados para trabajar datos tidy


      table <- read_csv("../Desktop/Directorio/CIDE/Trabajos de R/Tidyversecurso/population.csv")
   
         View(table)

      table %>% 
          mutate(rate = cases/population *10000)

      table %>% 
        count(year, wt = cases)
      
      table %>% 
        ggplot(aes(year, cases)) +
        geom_line(aes(group = country), color = "grey") + 
        geom_point(aes(color = country))
      

     # El código de esta sección sobre gather y spread 
      # ha cambiado por pivot_long y pivot _wider respectivamente
      
     #  https://r4ds.had.co.nz/tidy-data.html#pivoting


      # Sesión 91: spreading and gathering
      
        #  ejemplos que están limpios y no 
          # gather : unir
          # spread : separar
      
      # Sesion 92: gathering
      
      table4a %>%  
        gather(`1999`, `2000`, key = "year", value = "cases" ) -> tidy4a
      
      table4b %>% 
        gather(`1999`, `2000`, key = "year", value = "population") -> tidy4b

      # Voy a juntar la tablas 
      left_join(tidy4a, tidy4b)      
      
      # gathering nos permite agrupar sin grandes complicaciones de tal manera de tener datos limpios
    
      # Sesion 94: spreading, necesitamos separar 
      
      table2 %>% 
        spread(key = type, value = count)
      
      
      # Sesin 95: técnica de separación
        
      table3
    # ejemplo de separate
    # * separar la info que e encuentra en una columna. 
    # * para la separación debemos utilizar el mismo cracater que separa la info en la col
      # un simboo facil de reconocer
      
      table3 %>% 
        separate(rate, into = c("cases","population")) # automaticamnete divide
                                                       # la info por este simbolo '/'
      
      table3 %>% 
        separate(rate, into = c("cases","population"), sep = "/")
                                                      
                                                        # reconoce siempre y cuando no sea
                                                        # un caracter alpha numérico
      
      table3 %>% 
        separate(rate, into = c("cases","population"),
                 sep = "/", 
                 convert = TRUE)
                                                        # convierte el tipo de datos 
      
      
                                                        # con convert
      
       # con separate también podemos dividir los enteros de una columna, como la variable year
      table3 %>% 
        separate(rate, into = c("cases","population"),
                 sep = "/", 
                 convert = TRUE) %>% 
        separate(year, sep = 2, into = c("century","year"), convert =TRUE)
      
      
      # Sesión 95: Técnica de reunión
      # ahora podemos hacer lo contrario, unir datos de variables
      
      table5 %>% 
        unite(new_year, century, year, sep = "")
      
      
      # Assigmnet 17
      
      
     # 1. Describe cómo están organizadas las variables y observaciones en cada una de las cinco 
      # famílias de tablas table1... table5 de tidyverse.
      
     # table1: son 4 variables: country, year, cases, population
      # hay dos observaciones para cada país, con número de casos y población total
      
     # table2: son 4 variables country, year, type, count. en este caso son 4 observaciones por país. dos por año, para casos y numero de población
       # la variable count representa dos grupos de valores
      
     # table3: soon tres variables: country, year y rate. dos observaciones por año. La variable rate está representada como un chr 
       # y debería representar el cociente entre dos valores que no aparacen en la tabla
      
     # table4a: Esta tabla representa a 3 variables country, año 99 y 2000. debería representar el número de casos
      
     # table4b: Esta tabla representa las observaciones de poblaciones para cada país en los años 99 y 2000.
      
     # table5: esta tabla representa a cuatro variables: country, century, year y rate. dos observaciones por año y century. La variable rate está representada como un chr 
      # y debería representar el cociente entre dos valores que no aparacen en la tabla
      
      
      # 2. Calcula la columna de rate para table2 y para la combinación de table4a y table4b 
      # sin usar las funciones gather o spread. 
      
        table2 %>% 
              filter(type == "cases") %>% 
                                       select(country,year,count) %>% 
                                            rename(cases = count )-> table2_cases
              
        table2 %>% 
          filter(type == "population") %>% 
                select(country,year,count) %>% 
                    rename(population = count )-> table2_population
        
        
        # voy a juntar las dos tablas
        
        left_join(table2_cases, table2_population) %>% 
                    mutate(rate = cases/population)
        
        
        #### Segundo ejercicio
        
        table4a # cases
        
        table4a %>% 
          rename(cases_99 = `1999`, cases_00 = `2000`) -> table4a_cases  
        
        table4b # population
        
        table4b %>% 
          rename(population_99 = `1999`, population_00 = `2000`) -> table4b_population
        
        # las junto y creo la variable rate
        
        
        left_join(table4a_cases,table4b_population) %>% 
           mutate(rate_99 = cases_99/population_99, rate_00 = cases_00/population_00)
        
        
        
      # 3. Recrea el plot que hemos hecho en la clase para mostrar los casos de infección
        # a lo largo de los años usando la table2 
      # en lugar de la table1. ¿En qué difiere el proceso?
        
        
        # el ejemplo fue
        table %>% 
          ggplot(aes(year, cases)) +
          geom_line(aes(group = country), color = "grey") + 
          geom_point(aes(color = country))
       
         # table2
            # en este caso debemos hacer un filtrado en la variable type y modificar la
            # la variable y que antes era cases, ahora será count
            # para obtener el mismo gráfico 
        
        table2 %>% 
          filter(type == "cases") %>% 
              ggplot(aes(year, count)) +
              geom_line(aes(group = country), color = 'grey') +
              geom_point(aes(color = country))
        
        
      # 4.  Las funciones spread y gather no son absolutamente simétricas.
        # Toma el siguiente ejemplo para explicarlo correctamente:
        
        roi <- tibble(
          year = c(rep(2016,4), rep(2017,4), 2018),
          quarter = c(rep(c(1,2,3,4),2),1),
          return = rnorm(9, mean = 0.5, sd = 1)
        )
      
      roi %>%
        spread(year, return) %>%
        gather("year", "return", `2016`:`2018`)
      
      # spread tiende a rellenar valores cuando no encuentra simetría en el núermo de observaciones
      # de la variable estudiada. en este caso 2016 y 2017 tienen 4 observaciones para los 4 quarter
      # entonces debe rellenar con valores desconocidos. GATHER solo reconoce y reordena aumentando
      # las filas respecto a la tabla original
      
      
      
      # 5. Las funciones de spread y gather comparten un argumento convert. Investiga su uso.
      
      
      ?spread
      
      # Convert a data object to logical, integer, numeric, complex, character or factor as appropriate.
      
      
      # 6. Sin ejecutar, investiga por qué falla el siguiente código
      
      table4a %>%
        gather(1999,2000, key = "year", value = "cases")
      
      # falla porque el nombre de columnas debe llevar el apóstofe '1999' y '2000'
      # entonces, no es capaz de reconocer las columnas 
      
      # 6. Explica porqué falla la función spread aplicada a la siguiente tribble:
        
        people <- tribble(
          ~name,         ~key,   ~value, ~year,
          #-------------|-------|-------|-------
          "Juan Gabriel", "age",     30,  8,
          "Juan Gabriel", "weight",  71,  9,
          "Juan Gabriel", "age",     18,  10,
          "Ricardo",      "age",     55,  11,
          "Ricardo",      "age",     75,  12,
        )
      
        people %>% 
        spread(name, year)
        
        
      # en este caso el error está en que cada fila deber ser identificada por 
        # una única salida. puede deberse a errores lógicos en las columnas
        
          
      # 7. ¿Crees que añadiendo alguna nueva columna se soluciona el problema? Di cual.
        
        
        # cambiando los valores de la variable key podemos lograr o añadiendo una variable de año
        # sería lo mejor
      
      # 8. Limpia la siguiente tribble con la función de spread o gather que creas más útil. 
      
      pregnancy <- tribble(
        ~pregnant, ~male, ~female,
        #--------|------|---------
        "yes",    NA,    32,
        "no",     85,    43
      )
      
      pregnancy %>% 
        gather(male, female, key = "gender", value = "count")
      
  
    
      # Indica cuales son las nuevas variables después de aplicar la función.
      
          # las variable son género y cantidad 
      
      # Investiga los parámetros extra y fill de la función separate.
      
     # fill	
      
     # If sep is a character vector, this controls what happens when there are not enough pieces. 
      #There are three valid options:
        
     # "warn" (the default): emit a warning and fill from the right
      
     # "right": fill with missing values on the right
      
     # "left": fill with missing values on the left
      
      df <- data.frame(x = c(9, "x.y", "x.z", "y.z"))
      df %>% separate(x, c("A", "B"), fill = "left")        
      
      ?separate
     
      df %>% separate(x, c("a", "b"), extra = "drop", fill = "right")
      
      # con fill = "right" sitúa el valor 'NA' en el df. Lo mismo para left
      
      
      
      # Con 'extra'
      	
      # If sep is a character vector, this controls what happens when there are too many pieces.
      # There are three valid options:
        
      #   "warn" (the default): emit a warning and drop extra values.
      
      #   "drop": drop any extra values without a warning.
      
      #   "merge": only splits at most length(into) times
      
            # To only split a specified number of times use extra = "merge":
      
      df <- data.frame(x = c("x: 123", "y: error: 7"))
      
      
      df %>% separate(x, c("key", "value"), ": ")
      
      
      df %>% separate(x, c("key", "value"), ": ", extra = "merge")
      
      # Experimenta con varias opciones de las mismas con las dos tibbles siguientes:
        
        tibble(x = c("a,b,c", "d,e,f,g","h,i,j")) %>%
        separate(x, c("x", "y", "z", "d"), fill = "right")
      
        
        tibble(x = c("a,b,c", "d,e","f,g,h")) %>%
        separate(x, c("x", "y", "z"), fill = 'left')
      
      # unite y separate tienen un argumento llamado remove. ¿Cómo funciona? ¿Se te ocurre cuando lo pondrías a false?
        
        
        ?unite
        
        # 	
        # remove : If TRUE, remove input columns from output data frame.
        
        
        df <- expand_grid(x = c("a", NA), y = c("b", NA))
        df
        df %>% unite("z", x:y, na.rm = TRUE, sep ="")
        df %>% unite("z", x:y, na.rm = TRUE, remove = FALSE)
        
        # remove, permite modificar la instrucción de pegado de columnas. Es decir, 
        # aparte de presengar la columna nueva, puede también mantener las preexistentes
        # como en el ejemplo anterior.
        
      #  Compara las funciones separate y unite.
        #¿Por qué existen tres variantes de separación (basándonos en posición, separador o por grupos) 
        # pero solamente una para unir?
        
        # ?separate: con esta puede cambiar el orden de los valores en la tabla, es indispensable la posición
        # ?unite : en esta es solo la "sep", para ver cómmo vas a empastar los datos de las cols
        
        
      #  Compara el argumento fill de spread y el de complete. 
        # Investiga también el argumento direction de la función fill. 
      
        
      ?spread  
     #    If set, missing values will be replaced with this value.
     #    Note that there are two types of missingness in the input: 
     #    explicit missing values (i.e. NA), and implicit missings, rows that simply aren't present.
     #    Both types of missing value will be replaced by fill. 
        
        # acá llenamos con NA's
        
      ?complete  
        
        ?fill
       	
       # A named list that for each variable supplies a single value to use instead
       #  of NA for missing combinations.
      
        # acá decidimos qué valores ponermos en los espacios desconocidos
        
        #ejemlplo
        
        df <- tibble(
          group = c(1:2, 1),
          item_id = c(1:2, 2),
          item_name = c("a", "b", "b"),
          value1 = 1:3,
          value2 = 4:6
        )
        
        df %>% complete(group, nesting(item_id, item_name))
        
        # You can also choose to fill in missing values
        df %>% complete(group, nesting(item_id, item_name),
                        fill = list(value1 = NA))
        
      #Sesión 96:  NA en la limpieza de datos
      
        
        # NA aparece de forma explícita; el dato falta.
        # Na de forma implícita. Fíjate que en la siguiente tabla hay datos "faltantes"   
        
        
      roi <- tibble (
        year = c(rep(2016,4), rep(2017,4), 2018),
        quarter = c(rep(c(1,2,3,4),2),1),
        return = rnorm(9, mean = 0.5, sd = 1)
        
      )    
      
        
        # voy a añadir un NA en la posicion 7 de la columna return
      
      roi$return[7]  = NA
      roi  
        
        # Salen a relucir los valores que faltan implícitamente con 'spread'
      
      roi %>%
        spread(year, return) %>% 
        gather(`2016`:`2018`, key = "year", value = "return")
      
        # podemos usar na.rm (de remove) para hacer desaparecer los NA
      
      roi %>%
        spread(year, return) %>% 
        gather(`2016`:`2018`, key = "year", value = "return", na.rm = TRUE)
      
        # otra manera que tenemos de hacer esto es a través de la f'n complete
          # * fíjate que rellenará los valores (de las combinaciones únicas) que hagan falta en la columna seleccionada
      
      roi %>% 
        complete(year, quarter) # aparecen los NA's que no faltaban
      
      
        # a veces hay NAs que deben considerarse como el mismo valor que la entrada anterior
      
      
      treatments <- tribble(
        ~name,            ~treatment,     ~ responde,
        "Juan Gabriel",            1,              8,
                    NA,            2,              10,
                    NA,            3,              4,
            "Ricardo" ,            1,              7,
                    NA,            2,              9,
                    NA,            1,              8,
        
      )
      
      # Con la f'n fill podemos rellenar con el último valor conocido
      
      
      treatments %>% 
        fill(name)
      
      # caso de sanidad de la realidad
      
      tidyr::who %>% 
        View()
      
      # por dónde empezar limpiando datos
        # * quedarnos con  variables, a veces sólo son valores 
        # * descartar variables redundantes
      
      tidyr::who %>% 
        gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE) -> who1 
      
      
      who1 %>% 
        count(key) %>%  View() # fíjate que la clave trae mucha información. vamos a modificar un poco
      
      
      who2 <- who1 %>% 
                  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) # * creamos la variable nueva key, modificamos newrel por new_rel de 
                                                                               # * de la columna 'key'
      who2 %>% 
        count(key) %>%  View()
      
      # note que ahora podemos dividir la columna 'key' en tres elementos
            
      
      who3 <-  who2 %>% 
                  separate(key,c( "new", "type", "sexage"), sep = "_")
       
      # fíjate que tengo columnas inútiles. las podemos hacer desaparecer
      
      
      who4 <- who3 %>% 
          select(-new, -iso2,-iso3)
      
      
      # ahora, podemos separar la columna sexage. Lo podemos hacer por posiición 
      
      who5 <- who4 %>% 
              separate(sexage, into = c("sex", "age"), sep = 1)
      # ahora tenemos un data set limpio, bien estruturado
       # * filas corresponden a obs
       # * columanas cprresponden a variables
       # * celdas correspopnden a valores
      
      # CONSTRUYENDO UNA PIPE COMPLEJA PARA CREAR
      
      tidyr::who %>% 
        gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE) %>% 
        mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>% 
        separate(key,c( "new", "type", "sexage"), sep = "_") %>%
        select(-new, -iso2,-iso3) %>% 
        separate(sexage, into = c("sex", "age"), sep = 1) -> who6
        
        
    # Sesión 98 : echarle un ojo a datos desestructurados
      
      
    # Sesión 99: datos relacionales
      
      nycflights13::airlines
      airports
      planes
      weather
      flights
      
    #  
    # clave: de forma unívoca identifica la observación de su propia tabla 
    # clave foránea:  es aquella que identifica una observacion en aquella tabla+
      
      # Ejemplos: clave primaria 
      #1.
          planes %>% 
            count(tailnum) %>% filter(n > 1)
      
      #2.
          weather %>% 
            count(year, month, day, hour, origin) %>% filter(n > 1)
          
      # si no hay claves podemos hacer un mutate
          
          
          # sesión 101: mutate joins
          
          # * permite combinar variables de dos tablas.
          # * busca obs que coincidan por clave, hace copia de una tabla hacia la otra
          # * 
          
          flights_new <- flights %>% 
            select(year:day, hour, origin, dest, tailnum, carrier)
          
          
          flights_new %>% 
            left_join(airlines, by = "carrier")
          
          flights_new %>% 
              mutate(name = airlines$name[match(carrier, airlines$carrier)])
          
      # Sesión 103: inner join 
          
          # * busca coincidencias exactas entre el primer data set y el segundo a traves de las claves
          
          
          
          x <- tribble(
            ~key, ~ value_x,
              1 , "x1",
              2 , "x2",
              3 , "x3",
          )
          
          y <- tribble(
            ~key, ~value_y,
                1, "y1",
                2, "y2",
                4, "y3",
          )
          
      # vamos a hacer un inner_join y notaremos que las filas que sobrevien son 
          # aquellas que comparten las misma 
          
          x %>% 
            inner_join(y, by ="key")
          
          # las filas que nop coinciden se eliminan de data set se excluyen
          
      # sesión 106: outer join. left, right full
      
              # left_join : se queda con todas las observaciones que aparecen een el primer 
               # * data set indepndioentemente si aparece en el segundo
          
              # right_join : se queda con todas las observaciones que aparecem en el primer
               # * data set independientemente si se ecnuentra en eñl primero
          
              # full_join  :se queda con todas las obs de ambos data set  
          
          
          x %>% 
            left_join(y, by = "key")
          
      
          x %>% 
            right_join(y, by = "key")
      
        
          x %>% 
            full_join(y, by = "key")

          
          
        # Sesión 107: qué ocurre con claves duplicdas
          
          
          x <- tribble(
            
            ~key, ~value_x,
            1, "x1",
            2, "x2",
            2, "x3",
            1, "x4"
          )
          
          
          y <- tribble(
            ~key, ~value_y, 
            1, "y1",
            2, "y2"
            
          )

          
          x %>% left_join(y, by = "key")          
          
          
          # qué ocurre si hay dos claves repetidas en los dos dfata sets
          
          
          x <- tribble(
            ~key, ~value_x,
            1, "x1",
            2, "x2",
            2, "x3",
            3, "x4"
          )
          
          
          y <- tribble(
            ~key, ~value_y,
            1, "y1",
            2, "y2",
            2, "y3",
            3, "y4"
          )
          
          
          left_join(x,y, by ="key")
          
          
        # Sesión 106: definir las columnas claves de los joins  
          
         
          flights_new %>% 
            left_join(weather) # natural join agregando valores null
          
          flights_new %>% left_join(planes, by = "tailnum")
          
          # si la variable tiene diferente nombre pero los mismos valores
          
          airports
          flights_new %>% left_join(airports, by = c("dest" = "faa"))
          
          
          flights_new %>%  left_join(airports, by = c("origin" = "faa"))
          
          
          
          #merge nociones de sql
    
          ?base::merge
                  
          # deplyr <-> base
          # * hemos visto los 4 join 
          # inner_join: <-> merge(x,y)
          # left_join: <-> merge(x,y, all.x= TRUE)
          # right_join <-> merge(x,y, all.y = TRUE)
          # full_join <-> merge(x,y,all.x=TRUE, all.y =TRUE)
          
          
          # dplyr <-> SQL

          
          # Sesion 107: filtering joins
          
          # *semi_join(x,y) -> se queda con las obs de x que tiene en correspondencia con y
          # *anti_join(x,y) -> eliminar triodas las observaciones que tienne una correspondencia en y
          
          
         # top ten de destinos más solicitadios
          
          flights %>% 
            count(dest, sort = TRUE) %>% 
            head(10) -> top_dest
          
          flights %>% 
            filter(dest %in% top_dest$dest)
          
          
          flights %>% 
            semi_join(top_dest)
          
          
          flights %>% anti_join(planes,  by = "tailnum") %>% 
            count(tailnum, sort = TRUE)
          
          
          # SESION 109: OPERACIONES ENTRE CONJUNTAS
          
          # intersect(x,y): observaciones comunes a x e y
          # union(x,y): observaciones unicas en x e y
          # setfiff(x,y): observaciones en x pero no en y (x-y)
          
          
          x <- tribble(
            ~a, ~b,
             1,  1,
             2,  1
                )
          y <- tribble(
            ~a, ~b,
             1,  1,
             1,  2
          )
          
          
           intersect(x,y)         
           union(x,y)
           setdiff(x,y)
           setdiff(y,x)
          
           
           # Sesión 111:  Los fundamentos de los strings
           
           s1 <- "esto es un string"
           s2 <- 'esto es un string que contiene otro "string" dentro'
           s3 <- "esto es un string sin borrar"
           
           
           double_quote <- "\"" # '"'
           single_quota <- '\'' # "'"
           backslash <- "\\"
           
           x <- c(single_quota, double_quote, backslash)
           writeLines(x)           
           
          
          ?writeLines
          
          #\n -> intro, salto de tabla
          #\t -> tabulador
           
           mu <- "\u00b5"
           
           # Manual de cómo utilizar las quotes
           
           ?'"'
           
           
           # Sesión 112: Operaciones con Strings (estos son del paquete base)
           
           str_length(c("hola ","", NA))
           
           str_c("a","b","c", sep =",")
           str_replace_na(c("a","NA"))
           
           x <- c("abc", NA)
           str_c("hola", str_replace_na(x), "adios", sep = " ")
          
           # añadir con sufijos y prefijos
           
           str_c("prefi-x", c("a","b", "c"), "-suffix")
           
           
           name <- "juan ga"
           momento_del_dia <- "tarde"
           birthday <- TRUE
           
           
           str_c(
             "Buena ", momento_del_dia, " ", name,
             if(birthday) "y Feliz Cumpleaños!!!", "."
           )
          
           # SI QUIERO COLPASARUN VECTOR EN UNA SOLA STRING
           
           
          str_c(c("a","b","c"), collapse = ",")
          
          # si quiero estraer partes de un string 
          
          # por ej, tengo un vector
          
          x <- c("Manzanas","Peras","Limones","Plátanos")
           str_sub(x, 1,3)
           str_sub(x, -3, -1)
           str_sub("x", 1,8)
           
           # en minúsculas
           
           str_sub(x,1,1) <- str_to_lower(str_sub(x,1,1))

           str_to_upper(x)   # convertir todas las letras en mayúsculas
           str_to_title(x)   # Convertir primera letra en mayúscula 
           
           str_to_upper("i", locale="tr") # ISO 639 abrev del idioma
           
           
           #Orden depende del lugar
           
           str_sort(c("apple","banana","eggplant"), locale = "haw")
           str_order(x, locale = "haw")
           
           
           # SESION 113: Las bases de las expresiones regulares
           
           #REGEXP
           
          # str_view()
          # str_view_all()
           
          x <- c("manzana", "banana","pera", "pomelo") 
          
          str_view(x, "an") # dónde aparece el patrón regular
          str_view(x, ".a.") # puntos para cualquier cosa
          dot <- "\\."
         
          writeLines(dot)          
          
          str_view(c("abc", "a.c", "bc."), dot)
          str_view(c("abc", "a.c", "bc."), "a\\.c")
         
          backslash <- "\\\\"
          
          writeLines(backslash)
           
        str_view("a\\b", "\\\\")
        
        
        
        ##
        df1 <- data.frame(x = c(1,1,2,3), y =1:4)
        df2 <- data.frame(x = c(1,1,2), z =1:3) 
        
        
    inner_join(df1,df2, by = 'x')
         
    ## Sesión 114: anclas y string
    
    
    # ^ -> inicio del string (comienza con determinada letra)
    # $ -> final del string
    
    # ejemplos: 
                str_view(x, "^p") # identifica el string que comienza con la letra 'p'
                str_view(x, "a$") # identifica el strinmg que termina con la letra 'a'
                
              
       y <- c("tarta de manzana", "manzana", "manzana al horno", "pastel de manzana")         
      str_view(y, "^manzana$")    
    
    
    #\b -> localizar la frontera de una palabra
      
      sum()
      summarise()
   #\d -> localizar cualquier dígito
   # \s cualquier espacio en blancp (espacio, tabulador, salto de )
   # [abc] -> localizar la a, b o c indistintamente.
   # [^abc] -> localizar cualquier cosa excepto la a, b o c     
      
   # abc|d..m, abc|xyz
   
  str_view(c("grey","gray"),"gr(e|a)y")      

  
       # Sesión 115: Repetición de grupos y referencias
  
  
  #? -> 0-1 aparece
  #+ -> 1 o + veces
  #* -> 0 o más veces
  
  
  x <- "El año 1988 es el más largo en número romanos: MDCCCLXXXIII"
  
  str_view(x, "CC?") 
  str_view(x, "CC+")
  str_view(x, "C[LX]+")
  
#  "colou?r"
# "ba(na)+"
  
  #{n} -> exactamente n repeticiones
  #{n,} -> n o más repeticiones 
  #{,m} -> como máximo m repeticiones
  #{n,m} -> entre n y m repeticiones 
  
  str_view(x, "C{2}")
  str_view(x, "C{2,}")
  str_view(x, "C{2,3}")
  str_view(x, "C{2,3}?") # agrgando el signo pregunta indica el resultado más corto
  str_view(x, "C[LX]+?")
  
  
  
  fruits = c("banana", "coco","papaya", "manzana", "pera", "pepino")
str_view(fruit,"(..)\\1", match = TRUE)      
      
      # Sesión 116: Encontrar y extraer coincidencias de la expresión regular


    # otras herramientas 

str_detect(fruits, "a") # vector lógico de salida


str_detect(words, "^j")
sum(str_detect(words, "^j"))
mean(str_detect(words, "[aeiou]$"))

sum(!str_detect(words,"[aeiou]"))
sum(str_detect(words, "^[^aeiou]+$"))

# palabras que no terminan en vocal
words[!str_detect(words,"[aeiou]")]

# palabras que terminan en vocal

       str_subset(words, "[aeiou]$")


df <- tibble(word = words, i = seq_along(words)) 


     df %>% filter(str_detect(words,"x$"))
    
     
     str_count(fruits, "a")
     
     
     mean(str_count(words, "[aeiou]"))
     
     
  df %>% 
    mutate(vowels = str_count(word, "[aeiou]"),
           consonants = str_count(word, "[^aeiou]"))
     
     str_count("abababababa", "aba")
     str_view_all("abababababa", "aba")
     
     
  # Agrupaciones y reemplazos de string
     
     # ocupa un paquete más complicado que viene en stringr
     
    head(sentences)
     
    length(sentences) 

    colors <- c("red","orange","yellow","green","blue","purple")     
     
    color_match <- str_c(colors, collapse = "|")
     
    has_color <- str_subset(sentences, color_match) 
     
    matches <- str_extract(has_color, color_match) # solo localiza la primera coincidencia
    
    
    
    more_than_one <- sentences[str_count(sentences, color_match) > 1]
    str_view_all(more_than_one, color_match) # 
    
    str_extract_all(more_than_one, color_match) # genera una lista de de listas 
    
    
    # simplify genera una matriz 
    
    str_extract_all(more_than_one, color_match, simplify = TRUE)
    
    
    X <-  c("x","x y", "x y z")
      
    str_extract_all(X, "[a-z]", simplify = TRUE)   # FÍJATE que el número de columnas equipara al número de coincidencias 
    
    
    noun <- "(a|the) ([^ ]+)" # una sentencia que comience con 'a' o 'the' seguido de un espacio seguido de una caracter 
                             # que no sea un espacio
    
    nouns <- sentences %>% 
              str_subset(noun) %>%
              str_extract(noun)            

              sentences %>% 
              str_subset(noun) %>%
              str_match(noun) %>%    #str_match coincidencia completa con cada una de las componenetes de la coincidencia
                head(20)
    # hay limitaciones, también recogemos adjetivos
              
              
              
              tibble(sentence = sentences) %>% 
                tidyr::extract(
                  sentence, c("article", "noun"),
                  "(a|the) ([^ ]+)",
                  remove = FALSE
                )
              
              
    # Sesión 118: divisiones y búsquedas de strings
              
         # vamos a reemplazar expresiones regulares 
              
    str_replace(fruits, '[aeiou]', '-') # reemplazar el primer caracter TRUE
    str_replace_all(fruits, '[aeiou]', '-')
              
        # dentro de un vector chico, otras cambios
    
    
    str_replace_all(c('1 coche', '2 teléfonos', '3 amigos'),
                    c('1' = 'un', '2' = 'dos', '3' = 'tres'))
    
    
    
    # De una oración vamos a cambiar la posición de las palabras
    
        sentences %>% 
                str_replace('([^ ]+) ([^ ]+) ([^ ]+)', '\\1 \\3 \\2') %>% 
                head(20)
    
     sentences %>%  
       head(10) %>% 
       str_split(' ')   # el resultado es una lista
     
     sentences %>%  
       head(10) %>% 
       str_split(' ') %>% 
       .[[1]]          # en esta lista solo me quedaré con un vector, el primero 
       
     'a,b,c,d,e' %>% 
       str_split(',') %>% 
       .[[1]]
     
     
     # añadiendo el parámetro simplify 
     
     sentences %>% 
          head(10) %>% 
          str_split(' ', simplify = TRUE)
     
     
     fields <- c('Name : Alonso Darío', 'Country : Chile', 'Age: 29')
     
     
     fields %>% str_split(': ', n = 2, simplify = TRUE)
       
     
     #
     sent <- 'Quiero romper el string'
     str_view_all('Quiero romper el string', boundary('character'))
     
     str_split(sent, ' ')
     
     # también podemos aplicarle el parámetro boundary a split
     
     str_split(sent, boundary('word'))[[1]] # agrgando este último nos genera un vector
     
     
     
     # quiero localizar la posición de una expresión, palabra o caracter
     str_locate_all(sent, "r")
     
     # o imagnia que quiero encontrar la exp regular exacta
     
     str_locate_all(sent, '[^aeiou] ')
     
     # ahora, si quiero generar substring
     
     str_sub(sent, 8,9)
     
     # sesión 119: otras expresiones regulares
     
     str_view(fruits, regex('na'))
     
     
     apples <- c('manzana', 'Manzana', 'MANZANA')
     
     # una búsqueda exacta de manzana sería
     
     str_view(apples, 'manzana')
     
     # si quiero que ignore mayúsculas y minúsculas
     
     str_view(apples, regex('manzana', ignore_case = TRUE))
     
     x <- 'Linea 1\nLinea 2\nLinea 3\nLinea 4'
     
    str_extract_all(x, '^Linea')[[1]]          
    
    str_extract_all(x, regex('^Linea', multiline = TRUE))  [[1]]
    
    
    phone <- regex('\\(?    # parentesis de apertura opcionales
                   (\\d{3}) # código de área  
                   [)- ]?   # cierre de paréntesis, guíon o espacio opcionales
                   (\\d{3}) # tres dígitos de teléfono
                   [ -]?     # espacio o guión opcional 
                   (\\d{3}) #tres dígitos finales
                   ', comments = TRUE)
    
    str_match('971-123-456', phone)
    
    # dotall=true <- permite que el '.' reemplace cualquier cosa
    
    
    library(microbenchmark)
    
    
    microbenchmark::microbenchmark(
      
      regex = str_detect(sentences, 'the'),
      fixed = str_detect(sentences, fixed('the')),
      times = 30
    )
    
    # queremos compartar si dos eexpresiones son iguales
    
    
    str_detect('\u00e1', fixed('a\u0301'))
    
    str_detect('\u00e1', coll('a\u0301')) # collation comparaciones insensitive
    
    
    turquish_i = c('I','İ','ı','i')
    
    str_subset(turquish_i, coll('i', ignore_case = TRUE, locale = 'tr'))
    
    
    
    install.packages('stringi')
    library(stringi)
    
    
    
    # quiero saber cçomo se está ejecutando R
    
              stringi::stri_locale_info()$Name
              
              
     str_view_all('esto es una frase', boundary('word'))
  str_extract_all('esto es una frase', boundary('word'))[[1]]
              
     # Sesion 120: buscar funciones, variables y directorios
  
 apropos('replace')
 apropos('fruit')  
 
 getwd()
 dir(pattern = '')
 glob2rx('*.Rmd')
          ?stringi    
 
 
 
      # Sesión 121: Factores
 
  # definir variables categóricas
 # string as factor un libro, porqué llega,os a utilizar tanto para factores. Forcats For categories
 
 
 # Sesión 122: Factores

 x1 <- c('Vie', 'Lun', 'Mar', 'Dom')
 sort(x1)

 # problema del orden
 
 
 
 day_levels <-  c('Lun', 'Mar', 'Mie', 'Jue', 'Vie', 'Sab', 'Dom')
 
 # voy a guardarlos con un orden 
 
 y1 <- factor(x1, levels = day_levels)
 
 sort(y1)
 
 
 # Qué ocurre si escribimos mal
 
 
 x2 <-c('Vim','Lun', 'Mar','Dom')

 y2 <- factor(x2, levels = day_levels) 
 
 y2 <- parse_factor(x2, levels = day_levels)
 
  
 f1 <- factor(x1, levels = unique(x1))
 
 
 f1 <-  x1 %>% 
   factor() %>% 
                  fct_inorder()
 
  # Sesión 123: Encuesta anual americana
 
    gss_cat %>% View()
 
  # un conteo de alguna variable de interés de este data set
    
    gss_cat %>% 
            count(race)
    
 
    # visualizamos los datos de este factor
    
    
    gss_cat %>%
      ggplot(aes(race)) + 
          geom_bar() +
      scale_x_discrete(drop = FALSE) # AGREGAMOS una categoría adicional 
      
    
    # Sesión 124: cambiar orden de los niveles de un factor
    
    religion_summary <- gss_cat %>% 
                         group_by(relig) %>% 
                            summarise(age = mean(age, na.rm = TRUE),
                                      tv_hours = mean(tvhours, na.rm = TRUE),
                                      n = n()
                                      )
    
    
        religion_summary %>% View()
   
  
        religion_summary %>% 
             ggplot(aes(tv_hours, relig)) + geom_point()
  
 
        
      # queremos ordenar las religiones para no ver una dispersión y de esta forma analizar mejor
        
        ggplot(religion_summary, aes(tv_hours, fct_reorder(relig, tv_hours))) +
          geom_point()
      
      
      # si queremos evitar aesthetics tan largas, con mutates se nos simplifica
        
        
        religion_summary %>% 
          mutate(relig = fct_reorder(relig, tv_hours)) %>% 
          ggplot(aes(tv_hours, relig)) +
          geom_point()
        
      # * ¿Ingresos guardan relación con las horas de tvs?
        
        
        income_summary <- gss_cat %>% 
          group_by(rincome) %>% 
          summarise(
            age = mean(age, na.rm = TRUE),
            Tv_hours = mean(tvhours, na.rm = TRUE),
            n = n()
          ) %>% 
          
        # mutate(rincome = fct_reorder(rincome,age)) %>% 
          ggplot(aes(age, rincome)) + geom_point()
        
        # nbo siemore usarv reorder es la opción
        
        
        # que pasa si me interesa eliminare algunas categorías para efectos de visual
        
        income_summary <- gss_cat %>% 
          group_by(rincome) %>% 
          summarise(
            age = mean(age, na.rm = TRUE),
            Tv_hours = mean(tvhours, na.rm = TRUE),
            n = n()
          ) %>% 
          
          mutate(rincome = fct_relevel(rincome, 'Not applicable')) %>% # nota que con relevel esa variable Not app... queda al final
          ggplot(aes(age, rincome)) + geom_point()
        
        
        
        
        # usando fct_reorder2, reorderna el factor de variables y`s con x
        
        
        
      by_age <- gss_cat %>% 
        filter(!is.na(age)) %>% 
        group_by(age, marital) %>% 
        count()
        
      
        ggplot(by_age, aes(age, n, color = marital)) + 
          geom_line(na.rm = TRUE)
        
        # FÍJATE que las leyendas no se condicen con el orden 
        
        
        ggplot(by_age, aes(age, n, color = fct_reorder2(marital, age, n))) +
          geom_line(na.rm =TRUE) + 
          labs(color = 'Marital')
        
        gss_cat %>% 
          mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%  # ordenamos los factores de forma creciente o decereciente
          ggplot(aes(marital)) +
          geom_bar()
        
        
        # Sesión 125: Mutar los niveles de los factores
        
        gss_cat %>% count(partyid)  # podemos recodificar estos niveles
        
        # recodificando los niveles
        gss_cat %>% 
          mutate(partyid = fct_recode(partyid,
            'Republican, strong'      = 'Strong republican',
            'Republican, weak'        = 'Not str republican',
            'Independent, near rep'   = 'Ind,near rep',
            'Independent, near dem'   = 'Ind,near dem',
            'Democrat, weak'          = 'Not str democrat',
            'Democrat, strong'        = 'Strong democrat',
            'Other'                   = 'No answer',
            'Other'                   = "Don't know",
            'Other'                   = "Other party"
          )) %>%  count(partyid)
        
        # Utilizando fct_collpse 
        
        
        gss_cat %>% 
          mutate(partyid = fct_collapse(partyid,
                  other = c('No answer', "Don't know", "Other party"),
                  rep   = c('Strong republican', 'Not str republican'),
                  dem   = c('Not str democrat','Strong democrat'),
                  ind   = c('Ind,near rep', 'Ind,near dem', 'Independent')
                                        
                                       )) %>%  count(partyid)
          
          # fct_lump crea categorías grandes, se pierde info
        gss_cat %>% 
          mutate(relig = fct_lump(relig)) %>%  count(relig)
          
          # pero se puede modificar, por ejemplo, identificando el número de categorías n + 1
        
        gss_cat %>% 
          mutate(relig = fct_lump(relig, n = 5)) %>%  count(relig, sort = TRUE)
        
        
        # Sesión 126: trabajar con fechas----
        
         # hay minutos de 61 segundos
         # * Paquete Lubridate
        
        # Sesión 127:  
        
        
      library(lubridate)
      
        
        # <date>
        # <time>
        # <dttm>
        
        #library(hms)
        
        
        # el día de hoy
        
        
        
        today()       
        
        now()        
       
        # procesando formatos
        
        ymd('2015-06-13')
        
        mdy('enero 30th, 2018')
        
        dmy('8-jun-2018')
        
        ymd(20180608)
          
        ymd_hms('2018-06-08 19:35:28') 
        
        mdy_hm('06/30/2017 05:30', tz = 'GMT')

       
        library(nycflights13)
         nycflights13::flights         
 
         
         # creando el objeto time
         
        flights %>% 
          select(year, month, day, hour, minute) %>% 
          mutate(departure = make_datetime(year, month, day, hour, minute))
 
    make_date_time_100  <- function(year, month, day, time){
      make_datetime(year, month, day, time %/% 100, time %% 100 )
    }     
    
    
    
    flights_dt <- flights %>% 
                  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
                  mutate(
                  dep_time = make_date_time_100(year, month, day, dep_time),
                  arr_time = make_date_time_100(year, month, day, arr_time),
                  sched_dep_time = make_date_time_100(year, month, day, sched_dep_time),
                  sched_arr_time = make_date_time_100(year, month, day, sched_arr_time)
                  ) %>% select(origin, dest,ends_with('delay'), ends_with('time'))
    
    flights_dt %>% View()
        
    
    
    flights_dt %>% 
      ggplot(aes(dep_time)) + 
      geom_freqpoly(bindwidth = 24*60*60) # un día
    
    
    flights_dt %>%  
      filter(dep_time < ymd(20130102)) %>% 
      ggplot(aes(dep_time)) + 
      geom_freqpoly(bindwidth = 600) 
    
    
     as_datetime(60*60) # cada unidad representa un segundo
    
     as_date(365*10 + 35) # cada unidad representa un día
     
     
     # EPOCH -> 1970-01-1
     
     # Sesión 128: obtener los componentes de una fecha
     
     nowwww<- now()
     
    year(nowwww)   
    month(nowwww)     
    mday(nowwww)
    yday(nowwww)
    wday(nowwww, label = TRUE, abbr = TRUE)
    hour(nowwww)
    minute(nowwww)
    second(nowwww)
     
    flights_dt %>% 
       mutate(wday = wday(dep_time, label = TRUE, abbr =FALSE)) %>% 
      ggplot(aes(x = wday)) + 
      geom_bar()
    
    
    flights_dt %>% 
      mutate(minute = minute(dep_time)) %>%
      group_by(minute) %>% 
      summarise(
        avg_delay = mean(arr_delay, na.rm =TRUE),
        n = n()) %>% 
          ggplot(aes(minute, avg_delay)) + 
          geom_line()
      
    
    flights_dt %>% 
      mutate(minute = minute(sched_dep_time)) %>% 
      group_by(minute) %>% 
      summarise(
        avg_delay = mean(arr_delay, na.rm = TRUE),
        n = n()
      ) %>% 
      ggplot(aes(minute, n))+
      geom_line()
    
      #Sesión 129: Redondeo y modificación de componentes de fecha
    
    
    # operaciones fecha
     # floor_date() redondeo hacia abajo
     # round_date() redondeo lo que salga
     #ceiling_date(redondeo hacia arriba)
    
    
    flights_dt %>% 
      count(week = floor_date(dep_time, 'week')) %>% 
      ggplot(aes(week,n)) +
      geom_line()
 
    
    # cambiar componentea componente
    
    d <- now()
    d
    
    year(d) <- 2030
    
    
    month(d) <- 02    
    d    
    
    hour(d) <- hour(d) + 3
    
    
    
    #« Crea una nueva fecha a partir de la origibal con los parámetros de interés
    update(d, year = 2020, month = 10, mday = 7, hour = 4)
    
    dmy('01022018') %>% 
      update(mday = 30)
    
    dmy('01022018') %>% 
      update(hour = 54321)
    
    flights_dt %>% 
      mutate(dep_hour = update(dep_time, yday = 1)) %>% # todos los vuelos del año hubieran salido en el día
      ggplot(aes(dep_hour)) +
      geom_freqpoly(binwidth = 300)
    
    
    # Sesión 130: Lapsos de tiempo
    
    # duraciones -> número exacto de segundos
    
    
    jb_age <- today() - ymd(19920427)
    
    class(jb_age)
    as.duration(jb_age)
    
    # el resultado lo guarda en segundos u la unidad más grande de tiempo
    
    dseconds(3250)
    dminutes(1024)
    dhours(c(12,24))
    ddays(5:10)
    dweeks(4)
    dyears(1)
    
    
    2 * dyears(1)
    dyears(1) + dweeks(13) + dhours(22)
    
    
    tomorrow <- today() +  ddays(1)
    
    last_year <- today() - dyears(1)
    
    one_pm <- ymd_hms('2016-03-12 13:00:00', tz='America/New_York')
    
    # ojo cuando sumamos elementos de tiempo
    
    one_pm  + ddays(1)
    
    
    # periodos -> unidades humanas(semanas, meses, años)
    
      # *con periodos es más fácil trabajar porque solo trabaja con unidades humanas
      # * le quitamos la 'd' inicial para proseguir con las operaciones
    
    one_pm + days(1)
    
    seconds(35)
    minutes(15)
    weeks(4)
    months(1:6)
    years(1)    
    
    10*months(6) + days(8)
    
    days(20) + hours(32) + minutes(45)
    
    ymd('2016-05-19') + years(2)
    
    
    flights_dt %>% 
      filter(arr_time < dep_time) %>% View()
    
    
    flights_dt %>% 
      mutate(
        overnight = arr_time < dep_time,
        arr_time = arr_time + days(overnight*1),
        sched_arr_time = sched_arr_time + days(overnight*1)
      ) -> flights_dt
    
    
    flights_dt %>% 
      filter(overnight, arr_time < dep_time)
    
    
    #intervalos -> punto de partida y de final
    
    
    dyears(1) / ddays(365)

    years(1)/days(1)   # este es una aproximación
    
    
    next_year <- today() + years(1)
    
    (today() %--% next_year) %/% days(1)
    
    
    # Sesión 131: zonas horarias
    
    # formas que se define las zonas horarias
    # IANA -> <CONTINENTE>/<CITY>
    # Europe/Madrid, Europe/Paris, America/New_York
    
    
    # con qué estoy trabajando en R
    Sys.timezone()
    
    # Lista de Times Zones
    OlsonNames()
    
    (x1 <- ymd_hms('2018-06-08 12:00:00', tz = 'America/New_York'))
    (x2 <- ymd_hms('2018-06-08 18:00:00', tz = 'Europe/Copenhagen'))
    (x3 <- ymd_hms('2018-06-09 04:00:00', tz = 'Pacific/Auckland'))
    
        x1-x2
        x2-x3    
    
    # UTC <-> GMT 
        
      x4  <- c(x1,x2,x3)

      x4a <- with_tz(x4, tzone = 'Europe/Madrid')         
        
        x4a -x4

    x4b <- force_tz(x4, tzone = 'Australia/Lord_Howe')    
        
    x4b - x4            
    
    
    
    
    
    
    