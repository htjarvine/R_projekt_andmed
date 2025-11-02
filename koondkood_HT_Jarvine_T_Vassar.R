#koondkood

#projekt - "Kliimamuutuste tajumine ja sellega seotud tegurid: võrdlev andmeanalüüs Euroopa riikide lõikes"
#Hanna-Triinu Järvine ja Tuuli Vassar 
#------------------------------------------------------------------

#ylesanne 1 kood
#Hanna-Triinu Järvine ja Tuuli Vassar 
#---------------------------------------------------------------------

link1 <- "https://raw.githubusercontent.com/htjarvine/R_projekt_andmed/main/andmed_originaal_HTJ_TV.csv"

# 1.ylesanne

koik_andmed1=read.csv(link1, encoding = "UTF-8")

andmed <- koik_andmed1 %>% select(domicil, clmchng,  cntry) %>%
  filter(!is.na(domicil), !is.na(clmchng), !is.na(cntry))

colnames(andmed) <- c ("elukoht", "kliimamuutus", "riik")

# andmete puhastamine:

andmed1 <- andmed %>%
  filter(elukoht %in% 1:5, kliimamuutus %in% 1:4)

# elukoha grupeerimine, lisan piirkonna veeru:

andmed1 <- andmed1 %>%
  mutate(piirkond = case_when(
    elukoht %in% c(1, 2) ~ "linnapiirkond",
    elukoht %in% c(3, 4, 5) ~ "maapiirkond"
  ))

# lisan kliimamuutuste kategooriate veeru

andmed1 <- andmed1 %>%
  mutate(kliimamuutus_kategooriad = case_when(
    kliimamuutus == 1 ~ "kindlasti muutub",
    kliimamuutus == 2 ~ "ilmselt muutub",
    kliimamuutus == 3 ~ "ilmselt ei muutu",
    kliimamuutus == 4 ~ "kindlasti ei muutu"
  ))

# kliimamure grupeerimine, lisan kliimamure veeru:

andmed1 <- andmed1 %>%
  mutate(kliimamure = case_when(
    kliimamuutus %in% c(1, 2) ~ "mures",
    kliimamuutus %in% c(3, 4) ~ "mittemures"
  ))

# kliimaamuutuste kategooriate jagunemine, sagedustabel

table(andmed1$piirkond, andmed1$kliimamuutus_kategooriad)

#                ilmselt ei muutu / ilmselt muutub / kindlasti ei muutu / kindlasti muutub
#linnapiirkond              110            803                 31             1256
#maapiirkond                 15            124                  2              200

# protsentides

prop.table(table(andmed1$piirkond, andmed1$kliimamuutus_kategooriad), margin = 1) * 100

#              ilmselt ei muutu / ilmselt muutub / kindlasti ei muutu / kindlasti muutub
# linnapiirkond        5.0000000     36.5000000          1.4090909       57.0909091
# maapiirkond          4.3988270     36.3636364          0.5865103       58.6510264

# statistiline test
chisq.test(table(andmed1$piirkond, andmed1$kliimamuutus_kategooriad))

# X-squared = 1.882, df = 3, p-value = 0.5972
# kuna p-väärtus ei ole < 0.05, siis erinevus ei ole statistiliselt oluline

#joonis - Kliimamuutuste tajumine linna- vs maapiirkonnas
ggplot(andmed1, aes(x = piirkond, fill = kliimamuutus_kategooriad)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("seagreen", "orange2", "steelblue4", "firebrick")) + 
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Kliimamuutuste tajumine linna- vs maapiirkonnas",
       x = "Elukoht",
       y = "Osakaal",
       fill = "Arvamus kliimamuutuse kohta") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, margin = margin(t = 10), colour = "black"),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), colour = "black"),
    axis.text.x = element_text(size = 12, margin = margin(r = 10), colour = "black"),
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 15)),
    legend.text = element_text(size = 11, colour = "black"),
    legend.title = element_text(size = 12, colour = "black"),
    legend.spacing = unit(15, "pt")
  )

#kui suur osa linna- ja maapiirkonna elanikest on mures?
#sagedustabel
table(andmed1$piirkond, andmed1$kliimamure)  #linnapiirkond, mittemures: 141; linnapiirkond, mures: 2059; maapiirkond, mittemures: 17; maapiirkond, mures: 324
#protsentides
prop.table(table(andmed1$piirkond, andmed1$kliimamure), margin = 1) * 100 #linnapiirkond, mittemures: 6.409091%; linnapiirkond, mures: 93.590909%; maapiirkond, mittemures: 4.985337%; maapiirkond, mures: 95.014663%

#statistiline test

chisq.test(table(andmed1$piirkond, andmed1$kliimamure))
#X-squared = 0.79666, df = 1, p-value = 0.3721
#kuna p-väärtus ei ole < 0.05, siis erinevus ei ole statistiliselt oluline

#joonis - Hinnang kliimamuutuste toimumisele linna- ja maapiirkonnas
library(ggplot2)
library(scales)

ggplot(andmed1, aes(x = piirkond, fill = kliimamure)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  labs(title = "Hinnang kliimamuutuste toimumisele linna- ja maapiirkonnas",
       x = "Elukoht",
       y = "Osakaal",
       fill = "Hinnang kliimamuutuste toimumisele") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, margin = margin(t = 10), colour = "black"),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), colour = "black"),
    axis.text.x = element_text(size = 12, margin = margin(r = 10), colour = "black"),
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 15)),
    legend.text = element_text(size = 11, colour = "black"),
    legend.title = element_text(size = 12, colour = "black"),
    legend.spacing.y = unit(10, "pt")
  )
#arvutan osakaalud riikide kaupa (uus tabel):
riikide_kaupa <- andmed1 %>%
  group_by(riik, piirkond, kliimamuutus_kategooriad) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(riik, piirkond) %>%
  mutate(percentage = n / sum(n) * 100)

#joonis - kliimamuutuste tajumine elukoha ja riigi järgi
ggplot(andmed1, aes(x = piirkond, fill = kliimamuutus_kategooriad)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("seagreen", "orange2", "steelblue4", "firebrick")) + 
  facet_wrap(~ riik) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Kliimamuutuste tajumine elukoha ja riigi järgi",
       x = "Elukoht",
       y = "Osakaal",
       fill = "Arvamus kliimamuutuse kohta",) + 
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, margin = margin(t = 10), colour = "black"),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), colour = "black"),
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 15)),
    legend.text = element_text(size = 11, colour = "black"),
    legend.title = element_text(size = 12, colour = "black"),
    legend.spacing.y = unit(10, "pt")
  )

#------------------------------------------------------------------------------)

#kysimus 2
#Hanna-Triinu Järvine ja Tuuli Vassar 

#Kas temperatuur ja õnnelikkus on omavahel seotud? 
#-------------------------------------------------------------------------------

#kysimus 2 vajaminevad libraryd
library(ggplot2)
library(dplyr)

#selgitused õnnelikkuse tulba kategooriate kohta

#happiness (happy)
#happy - How happy are you
#0	Extremely unhappy
#1	1
#2	2
#3	3
#4	4
#5	5
#6	6
#7	7
#8	8
#9	9
#10	Extremely happy
#77	Refusal*
#88	Don't know*
#99	No answer*

#kasutasin temperatuurimõõdikuna kuu aega enne küsimustikule vastamise keskmist temperatuuri
#seega uurime, kas viimase kuu temperatuur mõjutas meeleolu (õnnelikkust)
#tmpdcam - Temperature in degrees Celcius, month average to date
#Rolling monthly (30 days up-to-and including date) average of regional average daily air temperature at 2m height, for 2016-2022. Unit of measure: °C
#---------------------------------------------------------------------------
#kys2 dataset tegemine

link2 <- "https://raw.githubusercontent.com/htjarvine/R_projekt_andmed/main/andmed_originaal_HTJ_TV.csv"
andmed2 <- read.csv(link2, encoding = "UTF-8")

#valin vajaminevad veerud 
#tmpdcam - temperatuur
#happy - õnnelikkus
kys2 <- andmed2 %>%
  select(tmpdcam, happy) %>%
  filter(!is.na(tmpdcam), !is.na(happy))
#View(kys2)

#muudame tulpade nimed ära
colnames(kys2) <- c("temperatuur", "onnelikkus")
#kontroll
head(kys2)
#-------------------------------------------------------------------------------
#jätame välja õnnelikkuse väärtused mis jäävad väljapoole 0-10 vahemikku
kys2 <- kys2 %>% filter(onnelikkus %in% 0:10)

#õnnelikkus järjestatud kategooriaks (ordinal factor), tasemed on 0 kuni 10
kys2$onnelikkus <- factor(kys2$onnelikkus, levels = 0:10, ordered = TRUE)

#statistika temperatuuri kohta
summary(kys2$temperatuur)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-8.163   3.101   7.186   8.284  12.876  24.957
mean(kys2$temperatuur, na.rm = TRUE)
#[1] 8.284325
sd(kys2$temperatuur, na.rm = TRUE)
#[1] 7.059589


#temperatuur õnnelikkuse alusel - kokkuvõte
aggregate(temperatuur ~ onnelikkus, data = kys2, 
          FUN = function(x) c(mean = mean(x), sd = sd(x), median = median(x), min = min(x), max = max(x)))
#   onnelikkus temperatuur.mean temperatuur.sd temperatuur.median temperatuur.min temperatuur.max
#1           0         9.256210       7.799159           8.369489       -4.221855       23.414343
#2           1        11.938942       4.952793          11.138423        3.351167       18.902363
#3           2         5.948324       6.875140           5.838480       -1.642983       24.362170
#4           3         6.606954       5.950775           6.931273       -5.630268       22.651241
#5           4         6.508727       6.457348           6.584022       -5.196195       22.651241
#6           5         7.660514       6.508062           6.594144       -5.196195       24.646230
#7           6         8.708876       7.086828           7.648559       -3.822722       24.439450
#8           7         8.207528       7.242393           6.943550       -5.780463       24.646230
#9           8         8.349954       7.357554           6.973529       -8.163246       24.956620
#10          9         8.320200       6.872042           7.328907       -5.630268       24.646230
#11         10         9.083848       6.513994           8.592816       -4.652047       24.646230

#-------------------------------------------------------------------------------
#info kogumine temperatuurivahemike jms kohta
summary(kys2$temperatuur)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-8.163   3.101   7.186   8.284  12.876  24.957
#seega saaksin teha vahemikud -10--5;-5-0;0-5;5-10;10-15;15-20;20-25
#sest min -8.168 suhteliselt -10 lähedal ja
#max 24.957 suhteliselt 25 lähedal, vahemikud arusaadavad ja loogilised

#jätame välja õnnelikkuse väärtused mis pole vahemikus 0-10 ja
#temperatuuri puuduvad väärtused
kys2 <- kys2 %>%
  filter(onnelikkus %in% 0:10, !is.na(temperatuur))

#õnnelikkuse väärtused võiks numbrilisteks muuta visualiseerimiseks
if (is.factor(kys2$onnelikkus)) {
  kys2$onnelikkus <- as.numeric(as.character(kys2$onnelikkus))}

#soovin teha 5 kraadised vahemikud, nii, et need hakkavad 0-st
breaks_seq <- seq(-10, ceiling(max(kys2$temperatuur)), by = 5)
kys2$temp_bin <- cut(kys2$temperatuur, breaks = breaks_seq, include.lowest = TRUE, right = FALSE)

#ei arvesta NA-sid
kys2 <- kys2 %>% filter(!is.na(temp_bin))

#keskmine õnnelikkus temperatuurivahemikus
mean_happiness <- kys2 %>%
  group_by(temp_bin) %>%
  summarise(mean_onnelikkus = mean(onnelikkus, na.rm = TRUE),
            count = n())

#graafik - keskmine õnnelikkus temperatuurivahemike kaupa (5 kraadi kaupa)
ggplot(mean_happiness, aes(x = temp_bin, y = mean_onnelikkus)) +
  geom_col(fill = "seagreen") +
  labs(title = "Keskmine õnnelikkus temperatuurivahemike kaupa (5°C sammuga)",
       x = "temperatuur (°C)",
       y = "keskmine õnnelikkus (0–10)") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, margin = margin(t = 10), colour = "black"),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), colour = "black"),
    axis.text.x = element_text(size = 12, hjust = 1),
    axis.text.y = element_text(size = 12, colour = "black"),
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 15)),
  )


#jitterplot
ggplot(kys2, aes(x = temperatuur, y = onnelikkus)) +
  geom_jitter(height = 0.2, width = 0, alpha = 0.4, color = "black") +
  labs(title = "õnnelikkuse sõltuvus temperatuurist",
       x = "Temperatuur (°C)",
       y = "Õnnelikkus (0–10)") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, margin = margin(t = 10), colour = "black"),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), colour = "black"),
    axis.text.x = element_text(size = 12, hjust = 1, colour = "black"),
    axis.text.y = element_text(size = 12, colour = "black"),
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 15))
  )

#-------------------------------------------------------------------------------
#teen kokkuvõtte, mille alusel tulpdiagramm
kokkuvote <- kys2 %>%
  group_by(onnelikkus) %>%
  summarise(
    arv = n(),
    keskmine_temp = mean(temperatuur, na.rm = TRUE),
    sd_temp = sd(temperatuur, na.rm = TRUE),
    mediaan_temp = median(temperatuur, na.rm = TRUE),
    min_temp = min(temperatuur, na.rm = TRUE),
    max_temp = max(temperatuur, na.rm = TRUE)
  )

kokkuvote$onnelikkus <- factor(kokkuvote$onnelikkus, levels = 0:10)

#tulpdiagramm
ggplot(kokkuvote, aes(x = onnelikkus, y = keskmine_temp)) +
  geom_col(fill = "seagreen") +
  labs(
    title = "Keskmine temperatuur ja õnnelikkus",
    x = "Õnnelikkus (0–10)",
    y = "Keskmine temperatuur (°C)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, margin = margin(t = 10), colour = "black"),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), colour = "black"),
    axis.text.x = element_text(size = 12, hjust = 1),
    axis.text.y = element_text(size = 12, colour = "black"),
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 15)),
  )
#-------------------------------------------------------------------------------

#kysimus 3
#Hanna-Triinu Järvine ja Tuuli Vassar 

#aktiveerin vajaminevad libraryd
library(tidyverse)  
library(ggplot2)    
library(readr)      
library(dplyr)      
library(summarytools)

#-------------------------------------------------------------------------------
#andmete sisse lugemine
link3 <- "https://raw.githubusercontent.com/htjarvine/R_projekt_andmed/main/andmed_originaal_HTJ_TV.csv"
andmed3 <- read.csv(link3, encoding = "UTF-8")

#kontrollin, kas andmed laadisid õigesti R-i
#View(andmed3)
#on õigesti, 
#NB! tabelit vaadates tuleb üleval noolele vajutada et edasi liikuda, sest 50 veeru kaupa tabel

#soovin saada infot andmetabeli kohta, st mis struktuur ja mõõdud(ridade ja veergude arv)
str(andmed3)
#struktuuriinfo - muutujate nimed, tüübid, väärtused
nrow(andmed3)
#ridade arv
ncol(andmed3)
#tulpade arv

kys3 <- andmed3 %>%
  select(clmchng, eisced, agea, cntry) %>%
  filter(!is.na(clmchng), !is.na(eisced), !is.na(agea), !is.na(cntry))
colnames(kys3) <- c("kliimamuutus", "haridus", "vanus", "riik") #veerunimede muutmine loetavuse huvides
head(kys3) #tabel tundub korras
#View(kys3)


#andmestiku suurus on haldamiseks natuke liiga suur, seega teen küsimustele vastamiseks eraldi tabelid
#-------------------------------------------------------------------------------
#küsimus 3 - Kas mure kliimamuutuste (clmchng) pärast erineb haridustaseme (eisced) või vanuse (age) järgi? 
#kys3 dataset nende 3 veeruga

#hariduse unikaalsed väärtused
unique(kys3$haridus)
#tulid numbrid, kuid tegelikult tähistavad numbrid tekstilisi väärtusi, muudame kategooriateks

#hariduse (eisced) kategooriad
#0	Not possible to harmonise into ES-ISCED
#1	less than lower secondary
#2	lower secondary
#3	lower tier upper secondary
#4	upper tier upper secondary
#5	advanced vocational, sub-degree
#6	lower tertiary education, BA level
#7	higher tertiary education, >= MA level
#55	Other
#77	Refusal
#88 Do not know
#99 No answer

haridus_kategooriad <- c(
  "0" = "pole rühmitatav",
  "1" = "alla põhihariduse",
  "2" = "põhiharidus",
  "3" = "keskhariduse alumine aste",
  "4" = "keskhariduse ülemine aste",
  "5" = "kõrgem kutseharidus",
  "6" = "bakalaureus",
  "7" = "magister või kõrgem",
  "55" = "muu",
  "77" = "keeldus vastamast",
  "88" = "ei tea",
  "99" = "pole vastust")

#muudame veerus ära kategooraiteks nr-d
kys3$haridus_kategooriad <- factor(kys3$haridus, levels = names(haridus_kategooriad), labels = haridus_kategooriad)
#levels = names(haridus_kategooriad) - milliste nr väärtustega siduda kategooriad
#labels = haridus_kategooriad - kategooriate määramine

head(kys3) #tundub korras olevat (vaatasin ka tabelit)
#View(table)

#kliimamuutuste veeruga võiks sama teha:
#clmchng - Do you think world's climate is changing
#kategooriad

#kliimamuutuste toimumise hinnangud
#1	kindlasti muutub
#2	ilmselt muutub
#3	ilmselt ei muutu
#4	kindlasti ei muutu
#7	keeldumine
#8	ei tea
#9	pole vastust

kliimamuutus_kategooriad <- c(
  "1" = "kindlasti muutub",
  "2" = "ilmselt muutub",
  "3" = "ilmselt ei muutu",
  "4" = "kindlasti ei muutu",
  "7" = "keeldumine",
  "8" = "ei tea",
  "9" = "pole vastust")

kys3$kliimamuutus_kategooriad <- factor(kys3$kliimamuutus, levels = names(kliimamuutus_kategooriad), labels = kliimamuutus_kategooriad)

kys3$age_group <- cut(kys3$vanus,
                      breaks = c(14, 24, 34, 44, 54, 64, 74, 84, Inf),
                      labels = c("15–24", "25–34", "35–44", "45–54", "55–64", "65–74", "75–84", "85+"),
                      right = FALSE)

#vaatame kokkuvõtet kys3-st
summary(kys3)
#-------------------------------------------------------------------------------
#JOONISED

#Sagedustabelid
sagedustabel3 <- table(kys3$haridus_kategooriad, kys3$kliimamuutus_kategooriad)
print(sagedustabel3)
prop_table <- prop.table(sagedustabel3, 1)
print(round(prop_table, 3))

#värvid järgneval leheküljel, joonisel kasutuses
#https://sape.inf.usi.ch/quick-reference/ggplot2/colour


#Joonis - kliimamuutuste hinnang haridustaseme järgi
ggplot(kys3 %>% filter(kliimamuutus %in% 1:4, haridus %in% 1:7),
       aes(x = haridus_kategooriad, fill = kliimamuutus_kategooriad)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("firebrick", "orange", "seagreen", "steelblue4")) + 
  labs(title = "Kliimamuutuste hinnang haridustaseme järgi",
       x = "Haridustase", y = "Osakaal", fill = "Hinnang") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, margin = margin(t = 10), colour = "black"),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), colour = "black"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, colour = "black"),
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 15)),
  )

#Joonis - kliimamuutuste hinnang vanusegrupi järgi
ggplot(kys3 %>% filter(kliimamuutus %in% 1:4),
       aes(x = age_group, fill = kliimamuutus_kategooriad)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("firebrick", "orange", "seagreen", "steelblue")) +
  labs(title = "Kliimamuutuste hinnang vanusegrupi järgi",
       x = "Vanusegrupp", y = "Osakaal", fill = "Hinnang") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, margin = margin(t = 10), colour = "black"),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), colour = "black"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, colour = "black"),
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 15)),
  )

#--------------------------------------------------------------------------------
#ANALÜÜS RIIGITI
kys3$riik <- andmed3$cntry
#View(kys3)

library(dplyr)
library(ggplot2)
library(forcats)

#Read the dataset
kys3_riik <- kys3
#View(kys3_riik)

#Muutujad faktortunnusteks
kys3_riik$clmchng <- factor(kys3_riik$kliimamuutus_kategooriad,
                            levels = c("kindlasti ei muutu", "ilmselt ei muutu", "ei tea", "ilmselt muutub", "kindlasti muutub"),
                            ordered = TRUE)
kys3_riik$eisced <- factor(kys3_riik$haridus_kategooriad,
                           levels = c("alla põhihariduse", "põhiharidus", "keskhariduse alumine aste", "keskhariduse ülemine aste",
                                      "kõrgem kutseharidus", "bakalaureus", "magister või kõrgem", "muu"),
                           ordered = TRUE)
kys3_riik$age_group <- factor(kys3_riik$age_group, ordered = TRUE)
kys3_riik$country <- factor(kys3_riik$riik)

#statistilised näitajad
summary(kys3_riik$clmchng)
summary(kys3_riik$eisced)
summary(kys3_riik$age_group)
summary(kys3_riik$country)

#Sagedustabel - hinnang kliimamuutusele hariduse ja vanuse alusel
table(kys3_riik$clmchng, kys3_riik$eisced)
table(kys3_riik$clmchng, kys3_riik$age_group)

#Kokkuvõte - riigi alusel
grouped_summary <- kys3_riik %>%
  group_by(country, eisced, age_group, clmchng) %>%
  summarise(count = n()) %>%
  ungroup()

#Hinnang kliimamuutuse toimumisele vanuse ja riigi alusel
ggplot(grouped_summary, aes(x = age_group, y = count, fill = clmchng)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~country) +
  labs(title = "Hinnang kliimamuutuse toimumisele vanuse ja riigi alusel",
       x = "vanus (vahemik)", y = "osakaal", fill = "hinnang kliimamuutusele") +
  theme_minimal(base_size = 12) +
  theme(
    plot.margin = unit(c(20, 20, 20, 20), "pt"),
    axis.text.x = element_text(angle = 45, hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, hjust=0.5),
    axis.title = element_text(size = 14)
  )
#-------------------------------------------------------------------------------

#kysimus4
#Hanna-Triinu Järvine ja Tuuli Vassar

#libraryd
library(tidyverse)
library(ggplot2)
library(readr)

link4 <- "https://raw.githubusercontent.com/htjarvine/R_projekt_andmed/main/andmed_originaal_HTJ_TV.csv"
andmed4 <- read.csv(link4, encoding = "UTF-8")


#(Is there a relationship between environmental concern (clmchng) and political interest (polintr)?)

#riikide lühendid
#AT - Austria  
#BE - Belgia  
#CZ - Tšehhi  
#DE - Saksamaa  
#ES - Hispaania 
#FR - Prantsusmaa  
#GB - Ühendkuningriik  
#HU - Ungari  
#NO - Norra  
#SE - Rootsi

kys4 <- andmed4 %>%
  select(clmchng, polintr) %>%
  filter(!is.na(clmchng), !is.na(polintr))

colnames(kys4) <- c("kliimamuutus", "pol")
#View(kys4)

kliimamuutus_kategooriad <- c(
  "1" = "kindlasti muutub",
  "2" = "ilmselt muutub",
  "3" = "ilmselt ei muutu",
  "4" = "kindlasti ei muutu",
  "7" = "keeldumine",
  "8" = "ei tea",
  "9" = "pole vastust")

kys4$kliimamuutus_kategooriad <- factor(kys4$kliimamuutus, levels = names(kliimamuutus_kategooriad), labels = kliimamuutus_kategooriad)

#View(kys4)

pol_kategooriad <- c(
  "1" = "väga huvitatud",
  "2" = "üsna huvitatud",
  "3" = "kergelt huvitatud",
  "4" = "üldse pole huvitatud",
  "7" = "keeldub vastamast",
  "8" = "ei tea",
  "9" = "pole vastust")

kys4$pol_kategooriad <- factor(kys4$pol, levels = names(pol_kategooriad), labels = pol_kategooriad)

kys4$riik <- andmed4$cntry
#-------------------------------------------------------------------------------
# joonis - tulpdiagramm, osakaalud
ggplot(kys4 %>% filter(kliimamuutus %in% 1:4, pol %in% 1:4),
       aes(x = pol_kategooriad, fill = kliimamuutus_kategooriad)) +
  geom_bar(position = "fill") +
  labs(title = "Kliimamuutuste hinnang poliitilise huvi järgi",
       x = "Poliitiline huvi", y = "Osakaal", fill = "Kliimamuutus") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#joonis - heatmap
library(reshape2)
heat_data <- as.data.frame(prop.table(table(kys4$pol_kategooriad, kys4$kliimamuutus_kategooriad)))
colnames(heat_data) <- c("Poliitiline_huvi", "Kliimamuutus", "osakaal")

#joonis - osakaalud: kliimamuutus vs poliitiline huvi
ggplot(heat_data, aes(x = Kliimamuutus, y = Poliitiline_huvi, fill = osakaal)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "firebrick") +
  labs(title = "Osakaalud: kliimamuutus vs poliitiline huvi",
       x = "Kliimamuutus", y = "Poliitiline huvi") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(size = 14, margin = margin(t = 10), colour = "black"),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), colour = "black"),
    axis.text.x.bottom = element_text(size = 11, margin = margin(r = 10), colour = "black"),
    axis.text.y.left = element_text(size = 11, margin = margin(r = 10), colour = "black")
  )

#-------------------------------------------------------------------------------
#riigipõhine analüüs

#Sagedustabelid
#sagedustabel - kliimamuutuste hinnang
kliima_sagedustabel <- table(kys4$riik, kys4$kliimamuutus_kategooriad)
print(kliima_sagedustabel)

#sagedustabel - poliitiline huvi
pol_sagedustabel <- table(kys4$riik, kys4$pol_kategooriad)
print(pol_sagedustabel)

#-----------------------------------------------
#JOONISED

#Kliimamuutuste hinnang riikide lõikes
ggplot(kys4 %>% filter(kliimamuutus_kategooriad %in% c("kindlasti muutub", "ilmselt muutub", "ilmselt ei muutu", "kindlasti ei muutu")),
       aes(x = riik, fill = kliimamuutus_kategooriad)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("firebrick", "orange", "seagreen", "steelblue")) +
  labs(title = "Kliimamuutuste hinnang riikide lõikes",
       x = "Riik", 
       y = "Osakaal", 
       fill = "Hinnang",
       caption = "Riikide lühendid: AT – Austria, BE – Belgia, CZ – Tšehhi, DE – Saksamaa, ES – Hispaania, FR – Prantsusmaa, GB – Ühendkuningriik, HU – Ungari, NO – Norra, SE – Rootsi") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(size = 14, margin = margin(t = 10), colour = "black"),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), colour = "black"),
    legend.text = element_text(size = 11, colour = "black"),
    legend.title = element_text(size = 12, colour = "black"),
    legend.spacing = unit(15, "pt"),
    legend.spacing.y = unit(10, "pt"),
    plot.caption = element_text(size = 10, hjust = 0)
  )

#joonis - Poliitiline huvi riikide lõikes
ggplot(kys4 %>% filter(pol_kategooriad %in% c("väga huvitatud", "üsna huvitatud", "kergelt huvitatud", "üldse pole huvitatud")),
       aes(x = riik, fill = pol_kategooriad)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("firebrick", "orange", "seagreen", "steelblue")) +
  labs(title = "Poliitiline huvi riikide lõikes",
       x = "Riik", 
       y = "Osakaal", 
       fill = "Poliitiline huvi",
       caption = "Riikide lühendid: AT – Austria, BE – Belgia, CZ – Tšehhi, DE – Saksamaa, ES – Hispaania, FR – Prantsusmaa, GB – Ühendkuningriik, HU – Ungari, NO – Norra, SE – Rootsi") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(size = 14, margin = margin(t = 10), colour = "black"),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), colour = "black"),
    legend.text = element_text(size = 11, colour = "black"),
    legend.title = element_text(size = 12, colour = "black"),
    legend.spacing = unit(15, "pt"),
    legend.spacing.y = unit(10, "pt"),
    plot.caption = element_text(size = 10, hjust = 0)
  )

#joonis - Kliimamuutuste hinnang ja poliitiline huvi riikide lõikes
ggplot(kys4 %>% filter(kliimamuutus_kategooriad %in% c("kindlasti muutub", "ilmselt muutub", "ilmselt ei muutu", "kindlasti ei muutu")),
       aes(x = riik, fill = kliimamuutus_kategooriad)) +
  geom_bar(position = "fill") +
  facet_wrap(~ pol_kategooriad) +
  scale_fill_manual(values = c("firebrick", "orange2", "seagreen", "steelblue")) +
  labs(
    title = "Kliimamuutuste hinnang ja poliitiline huvi riikide lõikes",
    x = "Riik",
    y = "Osakaal",
    fill = "Kliimamuutus",
    caption = "Riikide lühendid: AT – Austria, BE – Belgia, CZ – Tšehhi, DE – Saksamaa, ES – Hispaania, FR – Prantsusmaa, GB – Ühendkuningriik, HU – Ungari, NO – Norra, SE – Rootsi"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(size = 14, margin = margin(t = 10), colour = "black"),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), colour = "black"),
    legend.text = element_text(size = 11, colour = "black"),
    legend.title = element_text(size = 12, colour = "black"),
    legend.spacing = unit(15, "pt"),
    legend.spacing.y = unit(10, "pt"),
    plot.caption = element_text(size = 10, hjust = 0)
  )
#-------------------------------------------------------------------------------



























