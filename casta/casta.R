##Title. Casta dorada##
##Author. Israel Solares##
##License. CC4##

library (tidyverse)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
####2020####
carrera<-read_csv("2020/carrera.csv") 
funcionarios<-read_csv("2020/funcionarios.csv")
funcionariosextra<-read_csv("2020/funcionariosextra.csv")
funcionariosextra<-subset(funcionariosextra, !str_detect(funcionariosextra$denominacion, "OTROS(\\s)INGRESOS"))
sni<-read_csv("2020/sni.csv")
sni2<-read.csv("2020/sni2019.csv")
sniresto<-read.csv("2020/sni2018.csv")
colnames(carrera)<-c("unidad", "nombre", "apellido1", "apellido2", "tipo", "bruta", "neta", "estimulos", "total")
carrera$id<-rownames(carrera)
carrera$nombre1<-str_extract(carrera$nombre, "^(\\w+)")
carrera$nombre2<-str_extract(carrera$nombre, "(?<=(\\s))(\\w+)")
carrera$estimulos<-carrera$estimulos+(((carrera$bruta*(73/30))++1090+995)/12)+1255
funcionarios$neta<-str_replace(funcionarios$neta, "[$]", "")
funcionarios$neta<-str_replace(funcionarios$neta, "(?<=(\\d))[,](?=(\\d))", "")
funcionarios$neta<-as.numeric(as.character(funcionarios$neta))
funcionariosextra$bruta<-str_replace(funcionariosextra$bruta, "[$]", "")
funcionariosextra$bruta<-str_replace(funcionariosextra$bruta, "(?<=(\\d))[,](?=(\\d))", "")
funcionariosextra$bruta<-as.numeric(as.character(funcionariosextra$bruta))
funcionariosextra$brutas<-ifelse(funcionariosextra$periodicidad=="ANUAL", funcionariosextra$bruta/12, 
                                 ifelse(funcionariosextra$periodicidad=="SEMESTRAL", funcionariosextra$bruta/6,  
                                        funcionariosextra$bruta))
extraf<-funcionariosextra %>%
  group_by(id) %>%
  summarize(estimulos=sum(brutas))
funcionarios<-left_join(funcionarios, extraf, by="id")
funcionarios$administrativo<-funcionarios$neta+funcionarios$estimulos
funcionarios$nombre1<-str_extract(funcionarios$nombre, "^(\\w+)")
funcionarios$nombre2<-str_extract(funcionarios$nombre, "(?<=(\\s))(\\w+)")
fun<-funcionarios %>%
  group_by(apellido1, apellido2, nombre) %>%
  summarize(netafun=sum(neta, na.rm = T), 
            estfun=sum(estimulos, na.rm=T))
carrera<-left_join(carrera, 
                   fun, by=c("apellido1", "apellido2", "nombre"))
colnames(sni)<-c("apellido1", "apellido2", "nombre", "nivel", "institucion", "area")
sni2$nombre<-ifelse(!is.na(str_extract(lag(sni2$text), "^(\\d+)$")), sni2$text, NA)
sni2$nivel<-ifelse(!is.na(str_extract(lead(sni2$text), "^(\\d+)$")), sni2$text, NA)
sni2<-fill(sni2, nombre, .direction="down")
sni2$nivel<-ifelse(!is.na(str_extract(sni2$nivel, "CANDIDAT")), "C", 
                   ifelse(!is.na(str_extract(sni2$nivel, "(\\b)I$")), 1, 
                          ifelse(!is.na(str_extract(sni2$nivel, "(\\b)II$")), 2, 
                                 ifelse(!is.na(str_extract(sni2$nivel, "(\\b)III$")), 3, NA))))
sni3<-subset(sni2, !is.na(sni2$nivel), select=c("text","nombre", "nivel"))
sni3<-separate(sni3, nombre, into=c("apellidos", "nombre"), sep=",")
sni3$nombre<-trimws(sni3$nombre, which = "both")
sni3$apellidos<-trimws(sni3$apellidos, which = "both")
sni3$name<-paste(sni3$apellidos, ", ", str_extract(sni3$nombre, "^(\\w+)"), sep="")
sniresto<-rbind(subset(sni3, select = c("name", "nivel")),subset(sniresto, select = c("name", "nivel")))
sniresto<-distinct(sniresto, name, .keep_all = T)
sni$name<-paste(sni$apellido1, ifelse(!is.na(sni$apellido2), paste(" ", sni$apellido2, ", ", sep=""), ", "), 
                str_extract(sni$nombre, "^(\\w+)"), sep="")
carrera$name<-paste(carrera$apellido1, ifelse(!is.na(carrera$apellido2), paste(" ", carrera$apellido2, ", ", sep=""), ", "), 
                    str_extract(carrera$nombre, "^(\\w+)"), sep="")
carrera$nombramiento<-str_replace_all(carrera$tipo, "^(\\w+)(\\s+)", "")
carrera$nombr<-str_to_title(str_extract(carrera$nombramiento, "EMERITO"))
carrera$nombr<-ifelse(is.na(carrera$nombr), 
                      str_to_title(str_extract(carrera$nombramiento, "EMERITO|TECNICO|PROFESOR|INVESTIGADOR")), 
                      carrera$nombr)
carrera2<-left_join(subset(carrera, select=c("id","name","unidad", "tipo", "bruta", "neta", "estimulos","estfun", "netafun","apellido1","nombre1","nombre2" , "nombre","apellido2", "nombr")),
                    subset(sni, select=c("apellido1", "apellido2", "nombre","nivel")), by=c("apellido1", "apellido2", "nombre"))
carrera2<-distinct(carrera2, id, .keep_all = T)
carrera2a<-subset(carrera2, !is.na(carrera2$nivel))
carrera2b<-subset(carrera2, is.na(carrera2$nivel))
carrera2c<-left_join(subset(carrera2b, select=-c(nivel)), sniresto , by="name")
carrerat<-rbind(carrera2a, carrera2c)
carreratest<-carrerat[which(!is.na(carrerat$nivel)),]
carrerat$sni<-ifelse(carrerat$nivel=="C", 8186.96,
                     ifelse(carrerat$nivel=="1", 14327.18,
                            ifelse(carrerat$nivel=="2", 18420.66,
                                   ifelse(carrerat$nivel=="3", 30701.10,0))))
carrerat$estimulos[is.na(carrerat$estimulos)]<-0
carrerat$sni[is.na(carrerat$sni)]<-0
carrerat$estfun[is.na(carrerat$estfun)]<-0
carrerat$netafun[is.na(carrerat$netafun)]<-0
carrerat$totalneto<-carrerat$neta+carrerat$estimulos+carrerat$sni+carrerat$estfun+carrerat$netafun
carrerat$unidad<-str_to_title(carrerat$unidad)
carrerat<-carrerat[order(carrerat$totalneto),]
carrerat$orden<-1:12882
write_csv(carrerat, "2020/t2020.csv")
####2021####
p2021 <- read_csv("2021/principal.csv")
a2021 <- read_csv("2021/Tabla_Apoyos.csv")
e2021 <- read_csv("2021/Tabla_Estimulos.csv")
g2021 <- read_csv("2021/Tabla_Gratificaciones.csv")
pe2021 <- read_csv("2021/Tabla_Percepciones.csv")
pe2021<-subset(pe2021, 
               !str_detect(pe2021$`DENOMINACIÓN DE LAS PERCEPCIONES ADICIONALES EN DINERO`, "OTROS(\\s)INGRESOS"))
pr2021 <- read_csv("2021/Tabla_Primas.csv")
re21 <- read_csv("2021/remuneraciones.csv")
a21 <- a2021 %>% 
  group_by(ID) %>%
  summarize(
    apoyos= sum(`MONTO NETO DE LOS APOYOS ECONÓMICOS`)
  )
e21 <- e2021 %>% 
  group_by(ID) %>%
  summarize(
    estifun= sum(`MONTO NETO DE LOS ESTÍMULOS`)/4
  )
g21 <- g2021 %>% 
  group_by(ID) %>%
  summarize(
    gratificaciones= sum(`MONTO NETO DE LAS GRATIFICACIONES`)/12
  )
pe21 <- pe2021 %>% 
  group_by(ID) %>%
  summarize(
    percepciones= sum(`MONTO NETO DE LAS PERCEPCIONES ADICIONALES EN DINERO`)
  )
pr21 <- pr2021 %>% 
  group_by(ID) %>%
  summarize(
    primas= sum(`MONTO NETO DE LAS PRIMAS`, na.rm = T)/6
  )
p21 <- p2021 %>% 
  group_by(ID, `PRIMER APELLIDO`, `SEGUNDO APELLIDO`, `NOMBRE (S)`) %>%
  summarize(
    salario= sum(`MONTO MENSUAL NETO DE LA REMUNERACIÓN  EN TABULADOR`, na.rm = T)
  )
colnames(p21)<- c("ID", "priapellido", "segapellido", "nombres", "salario")
p21<- left_join(p21, a21)
p21<- left_join(p21, e21)
p21<- left_join(p21, g21)
p21<- left_join(p21, pe21)
p21<- left_join(p21, pr21)
p21$apoyos[is.na(p21$apoyos)]<-0
p21$estifun[is.na(p21$estifun)]<-0
p21$gratificaciones[is.na(p21$gratificaciones)]<-0
p21$percepciones[is.na(p21$percepciones)]<-0
p21$primas[is.na(p21$primas)]<-0
fun21<- p21 %>% 
  group_by(priapellido, segapellido, nombres) %>%
  summarize(
    remufun= sum(salario),
    apoyos= sum(apoyos),
    estifun= sum(estifun),
    gratificaciones= sum(gratificaciones),
    percepciones= sum(percepciones),
    primas= sum(primas)
  )
fun21$estfun <- fun21$estifun + fun21$apoyos+fun21$gratificaciones+fun21$percepciones+fun21$primas
re21<- subset(re21, select=c("Unidad académica", "Primer apellido del profesor/a", "Segundo apellido del profesor/a",
                        "Nombre completo del profesor/a", "Remuneración neta",  "Remuneración bruta",
                       "Estímulos correspondientes a los niveles de contratación"))
colnames(re21) <- c("unidad", "priapellido", "segapellido", "nombres", "remuprof", "bruta","estprof")
re21$bruta<- str_replace(re21$bruta, "[$]", "")
re21$bruta<- str_replace(re21$bruta, "[,]", "")
re21$bruta <-  as.numeric(as.character(re21$bruta))
re21$estprof<- re21$estprof+(((re21$bruta*(73/30))+1044+1155)/12)+1443
re21$bruta<-NULL
re21$remuprof<- str_replace(re21$remuprof, "[$]", "")
re21$remuprof<- str_replace(re21$remuprof, "[,]", "")
re21$remuprof <-  as.numeric(as.character(re21$remuprof))
t21<-left_join(re21, 
                   subset(fun21, select=c("priapellido", "segapellido", "nombres", "remufun", "estfun"))
                   )
t21$remufun[which(is.na(t21$remufun))]<-0
t21$estfun[which(is.na(t21$estfun))]<-0
sni<-read.csv("2021/sni.csv", na.strings=c("", "sin apellido Materno"))
sni$nombre<-str_replace(sni$NOMBRE, "[,]", ", ")
sni$nombre<-str_replace(sni$nombre, "(?<=(\\w))(\\s)$", "")
sni$apellidos<-str_replace(sni$nombre, "[,].*$", "")
sni$nivel<- sni$CATEGORÍA
sni$nombres<- str_replace(sni$nombre, ".*[,](\\s)", "")
sni$nombres<- trimws(sni$nombres, which = "both")
snitotal<-subset(sni, select=c("nombre", "apellidos","nombres" , "nivel"))
snitotal<-distinct(snitotal, nombre, .keep_all = T)
snitotal$remusni <-ifelse(snitotal$nivel=="C", 3*2724.45, 
                          ifelse(snitotal$nivel=="1", 6*2724.45, 
                                 ifelse(snitotal$nivel=="2", 8*2724.45, 
                                        ifelse(snitotal$nivel=="3", 14*2724.45, NA)
                                 )
                          )
)
snitotal$priapellido<- ifelse(str_detect(snitotal$apellidos, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)$"),
                              str_extract(snitotal$apellidos, "^(\\w+)(\\s)(\\w+)"),
                              str_extract(snitotal$apellidos, "^(\\w+)"))
snitotal$segapellido<- ifelse(str_detect(snitotal$apellidos, "(\\w+)(\\s)(\\w+)$"), 
                              str_extract(snitotal$apellidos, "(\\w+)$"), NA)
snitotal<-distinct(snitotal, nombre, .keep_all=T)
total21<-left_join(t21, 
                   subset(snitotal, select=c("priapellido", "segapellido", "nombres", "remusni")), 
                   multiple=NULL)
total21$remusni[which(is.na(total21$remusni))]<- 0
total21$total <-total21$remuprof+total21$estprof+total21$remufun+total21$estfun+total21$remusni
write_csv(total21, "2021/t2021.csv")
####2022####
p2022 <- read_csv("2022/principal.csv")
a2022 <- read_csv("2022/Tabla_Apoyos.csv")
e2022 <- read_csv("2022/Tabla_Estimulos.csv")
g2022 <- read_csv("2022/Tabla_Gratificaciones.csv")
pe2022 <- read_csv("2022/Tabla_Percepciones.csv")
pe2022<-subset(pe2022, 
               !str_detect(pe2022$`DENOMINACIÓN DE LAS PERCEPCIONES ADICIONALES EN DINERO`, "OTROS(\\s)INGRESOS"))
pr2022 <- read_csv("2022/Tabla_Primas.csv")
re22 <- read_csv("2022/remuneraciones.csv")
a22 <- a2022 %>% 
  group_by(ID) %>%
  summarize(
    apoyos= sum(`MONTO NETO DE LOS APOYOS ECONÓMICOS`)
  )
e22 <- e2022 %>% 
  group_by(ID) %>%
  summarize(
    estifun= sum(`MONTO NETO DE LOS ESTÍMULOS`)/4
  )
g22 <- g2022 %>% 
  group_by(ID) %>%
  summarize(
    gratificaciones= sum(`MONTO NETO DE LAS GRATIFICACIONES`)/12
  )
pe22 <- pe2022 %>% 
  group_by(ID) %>%
  summarize(
    percepciones= sum(`MONTO NETO DE LAS PERCEPCIONES ADICIONALES EN DINERO`)
  )
pr22 <- pr2022 %>% 
  group_by(ID) %>%
  summarize(
    primas= sum(`MONTO NETO DE LAS PRIMAS`, na.rm = T)/6
  )
p22 <- p2022 %>% 
  group_by(ID, `PRIMER APELLIDO`, `SEGUNDO APELLIDO`, `NOMBRE (S)`) %>%
  summarize(
    salario= sum(`MONTO MENSUAL NETO DE LA REMUNERACIÓN  EN TABULADOR`, na.rm = T)
  )
colnames(p22)<- c("ID", "priapellido", "segapellido", "nombres", "salario")
p22<- left_join(p22, a22)
p22<- left_join(p22, e22)
p22<- left_join(p22, g22)
p22<- left_join(p22, pe22)
p22<- left_join(p22, pr22)
p22$apoyos[is.na(p22$apoyos)]<-0
p22$estifun[is.na(p22$estifun)]<-0
p22$gratificaciones[is.na(p22$gratificaciones)]<-0
p22$percepciones[is.na(p22$percepciones)]<-0
p22$primas[is.na(p22$primas)]<-0
fun22<- p22 %>% 
  group_by(priapellido, segapellido, nombres) %>%
  summarize(
    remufun= sum(salario),
    apoyos= sum(apoyos),
    estifun= sum(estifun),
    gratificaciones= sum(gratificaciones),
    percepciones= sum(percepciones),
    primas= sum(primas)
  )
fun22$estfun <- fun22$estifun + fun22$apoyos+fun22$gratificaciones+fun22$percepciones+fun22$primas
re22<- subset(re22, select=c("Unidad académica", "Primer apellido del profesor/a", "Segundo apellido del profesor/a",
                             "Nombre completo del profesor/a", "Remuneración neta",  "Remuneración bruta",
                             "Estímulos correspondientes a los niveles de contratación"))
colnames(re22) <- c("unidad", "priapellido", "segapellido", "nombres", "remuprof", "bruta","estprof")
re22$bruta<- str_replace(re22$bruta, "[$]", "")
re22$bruta<- str_replace(re22$bruta, "[,]", "")
re22$bruta <-  as.numeric(as.character(re22$bruta))
re22$estprof<- re22$estprof+(((re22$bruta*(73/30))+115+1044)/12)+1443
re22$bruta<-NULL
re22$remuprof<- str_replace(re22$remuprof, "[$]", "")
re22$remuprof<- str_replace(re22$remuprof, "[,]", "")
re22$remuprof <-  as.numeric(as.character(re22$remuprof))
t22<-left_join(re22, 
               subset(fun22, select=c("priapellido", "segapellido", "nombres", "remufun", "estfun"))
)
t22$remufun[which(is.na(t22$remufun))]<-0
t22$estfun[which(is.na(t22$estfun))]<-0
sni<-read.csv("2022/sni.csv", na.strings=c("", "sin apellido Materno"))
sni$nombre<-str_replace(sni$NOMBRE, "[,]", ", ")
sni$nombre<-str_replace(sni$nombre, "(?<=(\\w))(\\s+)$", "")
sni$apellidos<-str_replace(sni$nombre, "[,].*$", "")
sni$NIVEL<- trimws(sni$NIVEL)
sni$nivel<- str_extract(sni$NIVEL, "()(\\w+)$")
sni$nivel<- str_replace(sni$nivel, "Nacional", "C")
sni$nombres<- str_replace(sni$nombre, ".*[,](\\s)", "")
sni$nombres<- trimws(sni$nombres, which = "both")
snitotal<-subset(sni, select=c("nombre", "apellidos","nombres" , "nivel"))
snitotal<-distinct(snitotal, nombre, .keep_all = T)
snitotal$remusni <-ifelse(snitotal$nivel=="C", 3*2925.09, 
                          ifelse(snitotal$nivel=="I", 6*2925.09, 
                                 ifelse(snitotal$nivel=="II", 8*2925.09, 
                                        ifelse(snitotal$nivel=="III", 14*2925.09, NA)
                                 )
                          )
)
snitotal$priapellido<- ifelse(str_detect(snitotal$apellidos, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)$"),
                              str_extract(snitotal$apellidos, "^(\\w+)(\\s)(\\w+)"),
                              str_extract(snitotal$apellidos, "^(\\w+)"))
snitotal$segapellido<- ifelse(str_detect(snitotal$apellidos, "(\\w+)(\\s)(\\w+)$"), 
                              str_extract(snitotal$apellidos, "(\\w+)$"), NA)
snitotal<-distinct(snitotal, nombre, .keep_all=T)
total22<-left_join(t22, 
                   subset(snitotal, select=c("priapellido", "segapellido", "nombres", "remusni")), 
                   multiple=NULL)
total22$remusni[which(is.na(total22$remusni))]<- 0
total22$total <-total22$remuprof+total22$estprof+total22$remufun+total22$estfun+total22$remusni
write_csv(total22, "2022/t2022.csv")

####total####
t20<-read_csv("2020/t2020.csv")
t21<-read_csv("2021/t2021.csv")
t22<-read_csv("2022/t2022.csv")
t20<-subset(t20, select=c("unidad", "apellido1", "apellido2","nombre", "nombr", "totalneto"))
t20$unidad<-str_to_upper(t20$unidad)
c20<-read_csv("2020/t2020.csv")
c21<-read_csv("2021/t2021.csv")
c22<-read_csv("2022/t2022.csv")
c20<-subset(c20, select=c("apellido1", "apellido2","nombre", "neta", "estimulos", "netafun", "estfun", "sni", "totalneto"))
colnames(c20)<-c("priapellido", "segapellido", "nombres", "netaprof", "estprof", "netafun", "estfun", "sni", "total")
c21<- subset(c21, select=c("priapellido", "segapellido", "nombres", "remuprof", "estprof",  "remufun", "estfun", "remusni", "total"))
colnames(c21)<-colnames(c20)
c22<- subset(c22, select=c("priapellido", "segapellido", "nombres", "remuprof", "estprof",  "remufun", "estfun", "remusni", "total"))
colnames(c22)<-colnames(c20)
c20<-c20[order(c20$total, decreasing=T),]
c20$orden<-1:length(c20$total)
c21<-c21[order(c21$total, decreasing=T),]
c21$orden<-1:length(c21$total)
c22<-c22[order(c22$total, decreasing=T),]
c22$orden<-1:length(c22$total)
c20$año<-2020
c21$año<-2021
c22$año<-2022
c20$pnpc<-1
c21$pnpc<-1.03535
c22$pnpc<-1.10855
#Distribucion actual
c22t<-subset(c22, select=c("orden", "netaprof", "estprof",  "netafun", "estfun", "sni"))
c22t<-pivot_longer(c22t, cols=-c("orden"), names_to="Tipo", values_to="Ingreso")
gene<-ggplot(c22t, aes(x=orden, y=Ingreso, fill=Tipo))+geom_col(position="stack")+
  scale_fill_discrete(name = "Tipo de ingreso", labels = c("Estimulos Funcionario", "Estimulos Cátedra", 
                                                           "Salario neto Funcionario", "Salario neto Cátedra", 
                                                           "SNI"))+
  geom_line(aes(y=112122+(92399/12)))+scale_x_reverse()+
  labs(x="Ranking de ingreso", title="Gráfico 2. Ingreso de Profesores e Investigadores de Carrera", 
       subtitle="UNAM, 2023", 
       caption="Nota: La línea negra denota el límite salarial del Gobierno Federal")+
  theme(plot.title = element_text(hjust = 0.5))
#el uno por ciento
colnames(t20)<-c("unidad", "priapellido", "segapellido", "nombres", "nombr", "tot20" )
con<-left_join(t20, 
               subset(t21, select=c("priapellido", "segapellido", "nombres", "total")), 
               multiple="first")
colnames(con)<- c("unidad", "priapellido", "segapellido", "nombres",
                  "nombr",  "tot20",  "tot21" )
con<-left_join(con, 
               subset(t22, select=c("priapellido", "segapellido", "nombres", "total")), 
               multiple="first")
colnames(con)<- c("unidad", "priapellido", "segapellido", "nombres",
                  "nombr",  "tot20",  "tot21", "tot22")
con<-subset(con, !is.na(con$tot21)&!is.na(con$tot22))
con<-con[order(con$tot22, decreasing=T),]
con$orden<-1:length(con$unidad)
eluno<-subset(con, con$orden<401)
eluno<-subset(eluno, select = c("unidad", "priapellido", "segapellido", "nombres",  "nombr"))
cb20<-left_join(eluno, c20, multiple="first")
cb21<-left_join(eluno, c21, multiple="first")
cb22<-left_join(eluno, c22, multiple="first")
total<-rbind(cb20, cb21, cb22)
#total<-total %>% mutate(across(6:11, ~.x/pnpc ))
total2<-pivot_longer(total, cols=c("netaprof", "estprof", "netafun", "estfun", "sni"), 
                    names_to = "tipo", values_to="ingreso")
promtot2<- total2 %>%
  group_by(año, tipo) %>%
  summarise(ingreso = mean(ingreso))
promtot<-total %>%
  group_by(año) %>%
  summarise(ingreso = mean(total))
amlo<-data.frame(nombre="Salario Máximo Gobierno Federal", tipo=NA, 
                 total=c(111990, 112122+(92399/12), 112122+(92399/12)), año=c(2020, 2021, 2022))
mayores<-data.frame(Año=c(2020, 2021, 2022), 
                    Número=c(length(c20$priapellido[which(c20$total>111990)]), 
                             length(c21$priapellido[which(c21$total>112122+(92399/12))]),
                             length(c22$priapellido[which(c22$total>112122+(92399/12))]))
)
ing<-ggplot(promtot2, aes(x=año, y=ingreso, fill=tipo))+geom_area(position="stack")+
  geom_line(data = amlo, aes(x=año, y=total))+
  scale_fill_discrete(name = "Tipo de ingreso", labels = c("Estimulos Funcionario", "Estimulos Cátedra", 
                                                           "Salario neto Funcionario", "Salario neto Cátedra", 
                                                           "SNI"))+
  scale_x_continuous(breaks=c(2020, 2021, 2022), labels=c("2020", "2021", "2022"))+
  labs(x="Año", y="Monto", title="Gráfico 4. Los ingresos medios del 1%", 
       caption = "Nota: La línea negra denota el Salario Máximo en el Gobierno Federal")+
  theme(plot.title = element_text(hjust = 0.5))
sup<-ggplot(mayores, aes(x=Año, y= Número))+geom_col()+
  labs(title="Gráfico 3. Profesores en la UNAM con salario superior al límite federal")+
  theme(plot.title = element_text(hjust = 0.5))
candi<-read_csv("candidatos.csv")
candi$c<-1
candi2<-left_join(total, candi)
candi2<-subset(candi2, candi2$c==1)
candi2$nombre<-paste(candi2$priapellido, " ", candi2$segapellido, ", ", candi2$nombres, sep="")%>% str_to_title()
candi<-subset(candi2, select=c("nombre", "orden", "total", "año"))
amlo<-data.frame(nombre="Salario Máximo Gobierno Federal", orden=NA, 
                 total=amlo$total, año=c(2020, 2021, 2022))
candi<-rbind(candi, amlo)
candg<-ggplot(candi, aes(x=año, y=total, label=orden, color=nombre))+geom_line(linewidth=1)+#geom_label()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Gráfico 5. Ranking de ingresos de candidatos a rectoría")+
  labs(x="Año", y="Ingreso", color="Nombre")+
  scale_x_continuous(breaks=c(2020, 2021, 2022), labels=c("2020", "2021", "2022"))+
  scale_color_manual(values=c("#E69F00", "#56B4E9","springgreen3","darkgray","deeppink2", "slateblue3","chocolate4","gray1", "red1"))
candg
jpeg("../ingmedios.jpeg", width = 900, height = 600, quality=100)
ing
dev.off()
jpeg("../ing23.jpeg", width = 900, height = 600, quality=100)
gene
dev.off()
jpeg("../rank.jpeg", width = 900, height = 600, quality=100)
candg
dev.off()
jpeg("../super.jpeg", width =900, height = 600, quality=100)
sup
dev.off()
pdf("../ingmedios.pdf",paper = "a4r", width = 0, height = 0)
ing
dev.off()
pdf("../ing23.pdf", paper = "a4r", width = 0, height = 0)
gene
dev.off()
pdf("../rank.pdf", paper = "a4r", width = 0, height = 0)
candg
dev.off()
pdf("../super.pdf", paper = "a4r", width = 0, height = 0)
sup
dev.off()


write_csv(promtot, "promtot.csv")
write_csv(promtot2, "promtot2.csv")
write_csv(total, "total.csv")