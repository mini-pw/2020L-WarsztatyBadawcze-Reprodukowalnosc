# Artykul opisuje pakiet "condvis" do "conditional visualization", czyli wizualizacji dzialania
  # modeli predykcyjnych, ktore maja wiele wymiarow(wiecej niz 2, bo do 2 mozna latwo wizualizowac)

# urzywane biblioteki:
install.packages("condvis")
library("condvis")
install.packages("e1071")
library("e1071")
install.packages("randomForest")
library("randomForest")
install.packages("mgcv")
library("mgcv")



# potrzebne dane: dane dotycza dzieci, ktore palily/nie palily nigdy i ich pojemnosci pluc.
data("fev", package = "covreg")

# dostosowanie danych
fev$gender <- factor(fev$male, 0:1, c("female", "male"))
fev$male <- NULL
fev$smoke <- factor(fev$smoke, 0:1, c("no", "yes"))


plot(fev ~ smoke, data = fev, xlab = "smokes", ylab = "FEV",
     col = "gray", boxwex = 0.35)
# na pierwszy rzut oka wyglada, jakby palenie powodowalo zwiekszanie pojemnosci pluc - nic bardziej mylnego.
plot(age ~ smoke, data = fev, xlab = "smokes", ylab = "AGE",
     col = "gray", boxwex = 0.35)
# Po glebszej analizie widzimy, ze dzieci oznaczone jako palace sa duzo starsze.
  # Nic w tym dziwnego, ale dla modelu moze to byc problem, bo moze sie nauczyc tej pierszej zaleznosci,
  # ze jesli pali, to mniejsze pluca, co jest bez sensu. Dlatego potrzebujemy narzedzia, ktore umozliwi
  # nam sprawdzenie, czy nauczony przez nas model wpada w takie puapki


# tworzenie przykladowego modelu
m1 <- svm(fev ~ gender + smoke + age + height, data = fev)

# wizualizacja
ceplot(data = fev, model = m1, sectionvars = "smoke", type = "shiny", # jesli nie dziala uzyj: type = "separate"
       xcplotpar = list(cex = 0.4))
# zielona kreska - model
# czarne kolka - prawdziwe dane z okolicy
# okienko scatterplot age/height - interaktywne do wyboru przeciecia
# distance treshold, distance function type - bedzie za chwile

# jak widzimy ten model jest dla niskiego wieku glupi (wpadl w puapke) -
  # przewiduje, ze palace dzieci maja wieksze pluca



# inny przyklad na tych samych danych
m3 <- list(
  RF = randomForest(fev ~ ., data = fev),
  lm = lm(fev ~ ., data = fev),
  gam = mgcv::gam(fev ~ smoke + gender + s(age) + s(height), data = fev))
# 3 modele na raz - porownujemy
ceplot(data = fev, model = m3, sectionvars = "smoke", type = "shiny",   # type = "separate"
       xcplotpar = list(cex = 0.4))
# Random Forest nadal sie myli, ale pozostale 2 radza sobie calkiem calkiem na wszystkich wartosciach
  # age i height. Radza sobie nawet na bardzo mlodych dzieciach.


# przyklad z wieksza iloscia danych
data("powerplant", package = "condvis")
# nastepna linijka moze zajac do 8 sekund
m4 <- list(
  svm = svm(PE ~ ., data = powerplant),
  gam = gam(PE ~ s(AT) + s(V) + s(AP) + s(RH),
            data = powerplant))

ceplot(data = powerplant, model = m4["svm"], sectionvars = "AT", type = "shiny") # type = "separate"
# zwrocmy uwage na punkty w tle.
  # jesli ustawimy distance theshold na 2 i zerkniemy na przyklad na przeciecie z AP=1030, V=60
  # mozemy odniesc wrazenie, ze model sie zle nauczyl, gdyz zielona krzywa nie przybliza czarnych punktow.
# Zatem dla tych danych wartosc distance theshold = 2 byla zbyt duza do poprawnej interpretacji.
  # Byc moze dla innych danych wartosc distance theshold = 1 bedzie juz zbyt duza.


# przyklad 3D, gdy sectionvars ma 2 wartosci
ceplot(data = powerplant, model = m4["svm"], sectionvars = c("AT", "V"), type = "separate", # 
       view3d = TRUE, threshold = 0.2)
# strzalkami obracajcie wykres




# przyklad z duza iloscia zmiennych
data("wine", package = "condvis")
wine$Class <- as.factor(wine$Class)
m5 <- randomForest(Class ~ Alcohol + Malic + Ash + Magnesium + Phenols +
                     Flavanoids, data = wine)

# dwa dwuwymiarowe ekrany, czyli to co znamy
ceplot(data = wine, model = m5, sectionvars = c("Alcohol", "Phenols"), type =
         "shiny")

# nowosc, kazda zmienna to paseczek. Aktualne ustawienie jest nieczytelne przez duza ilosc wyswietlanych obserwacji
ceplot(data = wine, model = m5, sectionvars = c("Alcohol", "Phenols"), type =
         "separate", selectortype = "pcp", threshold = 1.5)

