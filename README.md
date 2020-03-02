# 2019L Warsztaty Badawcze Reprodukowalność

## [Wykład Warsztatów Badawczych](https://github.com/mini-pw/2020L-WarsztatyBadawcze)

## Plan Zajęć

* 2020-02-25 - O co chodzi z artykułami naukowymi?
    - [ggplot2 Compatible Quantile-Quantile Plots in R](https://journal.r-project.org/archive/2018/RJ-2018-051/RJ-2018-051.pdf)
    - [Dot-Pipe: an S3 Extensible Pipe for R](https://journal.r-project.org/archive/2018/RJ-2018-042/RJ-2018-042.pdf)

* 2020-03-03 - Po co nam reprodukowalność? + Podział prezentacjami
    - [Quantifying Independently Reproducible Machine Learning](https://thegradient.pub/independently-reproducible-machine-learning/)
    - [A Step Toward Quantifying Independently Reproducible Machine Learning Research](https://arxiv.org/pdf/1909.06674.pdf)
    - [Open Science in Software Engineering](https://arxiv.org/pdf/1904.06499.pdf)
    - [DATA 598 A Wi 20: Special Topics In Data Science: Reproducibility for Data Science](https://canvas.uw.edu/courses/1354201)

* 2020-03-10 - prezentacje I
    
* 2020-03-17 - prezentacje II

* 2020-03-24 - Praca Domowa I + projekt

* 2020-03-31 - [bookdown](https://bookdown.org/) + Praca Domowa II + projekt

* 2020-04-07 - Praca Domowa III + projekt

* 2020-04-21 - projekt

* 2020-04-28 - Prezentacja metodologicznej części projektu I

* 2020-05-05 - Prezentacja metodologicznej części projektu II

* 2020-05-19 - projekt (termin oddania opisu części metodologicznej projektu)

* 2020-05-26 - projekt

* 2020-06-02 - projekt (termin oddania całego projektu (artykułu))

* 2020-06-09 - ?

* 2020-06-16 - ?


## Prezentacje (15 pkt.)

Prezentacje trzeba wykonać w parach.
Należy wybrać artykuł z listy:

- [Integration of networks and pathways with StarBioTrek package](https://journal.r-project.org/archive/2019/RJ-2019-025/index.html)
- [stplanr: A Package for Transport Planning](https://journal.r-project.org/archive/2018/RJ-2018-053/index.html)
- [RcppMsgPack: MessagePack Headers and Interface Functions for R](https://journal.r-project.org/archive/2018/RJ-2018-068/index.html)
- [Geospatial Point Density](https://journal.r-project.org/archive/2018/RJ-2018-061/index.html)
- [neuralnet: Training of Neural Networks](https://journal.r-project.org/archive/2010/RJ-2010-006/index.html)
- [Mapping and Measuring Country Shapes](https://journal.r-project.org/archive/2010/RJ-2010-004/index.html)
- [tmap: Thematic Maps in R](https://www.jstatsoft.org/article/view/v084i06)
- [Conditional Visualization for Statistical Models: An Introduction to the condvis Package in R](https://www.jstatsoft.org/article/view/v081i05)
- [Enhancing Reproducibility and Collaboration via Management of R Package Cohorts](https://www.jstatsoft.org/article/view/v082i01)
- [archivist: An R Package for Managing, Recording and Restoring Data Analysis Results](https://www.jstatsoft.org/article/view/v082i11)
- [The Generalized Pairs Plot](https://www.tandfonline.com/doi/full/10.1080/10618600.2012.694762)
- [Visualizing Complex Data With Embedded Plots](https://amstat.tandfonline.com/doi/full/10.1080/10618600.2014.896808)

Prezentacja powinna trwać do 12 min. + 3 min. na dyskusję.
Powinna zawierać wprowadzenie do tematu i opis wyników arytkułu oraz informację, czy udało się zreprodukować wyniki (dyskusja o problemach i spostrzeżeniach dotczących reprodukowalności mile widziana).

Przed rozpoczęciem zajęć proszę o PR slajdów (w PDF) do folderu Prezentacje.

## Prace Domowe (15 pkt.)

Na każdą pracę domową jest czas do momentu rozpoczęcia kolejnych zajęć.


### Praca Domowa I (7 pkt.)
Spróbować (może się nie udać) zreprodukować co najmniej trzy wybrane artykuły naukowe (5 pkt. + 2 pkt. za znalezienie niereprodukowalnego i zidentyfikowanie przyczyny). Wyniki przesłać w formie raportu (pull request do folderu PD).

Journale do wybioru. Mozna wziąć artykuły z jednego lub kilku.
- [The R Journal](https://journal.r-project.org/)
- [Machine Learning Open Source Software in Journal of Machine Learning Research](http://www.jmlr.org/mloss/)
- [Journal of Computational and Graphical Statistics](https://www.tandfonline.com/toc/ucgs20/current)

### Praca Domowa II (3 pkt.)
Założenie podrozdziału odpowiadającego artykułowi, który powstanie w ramach projektu. Wybrać deskryptywny tytuł artykułu.

### Praca Domowa III (5 pkt.)
Napisać Related Work artykułów dotyczących reprodukowalności.
Umieścić Related Work w książce w odpowiednim artykule.

## Projekt (55 pkt.)

Celem projektu jest wykonanie analizy artykułów naukowych pod kątem reprodukowalności. Projekt trzeba wykonać w grupie 3-osobowej.

Wynikiem projektu powinien być krótki artykuł naukowy (40 pkt.), minimum 3 strony umieszczony jako rozdział książki online, która powstanie w ramach przedmiotu.
Podział punktów w ramach artykułu
- Abstrakt: 5 pkt.
- Introduction + Motivation: 10 pkt
- Opis metodologii i wyników: 15 pkt. (oddanie po 5.05.2020 max 10 pkt.)
- Wnioski: 10 pkt.

Projekt nalezy zaprezentować w postaci Lightning Talka na jednym z ostatnich wykładów (15 pkt.).

Pomysły na pytania badawcze, najlepiej uwzględnić kilka:
- Jak zmierzyć reprodukowalność? Czy jest zero-jedynkowa? A może procentowa?
- Czy są różne rodzaje niereprodukowalności? Czy można zaproponować jakąś klasyfikację problemów z odtwarzaniem wyników?
- Jak zmienia się reprodukowalność w zależności od roku wydania artykułu, czasopisma, języka progamowania?
- Może ograniczyć analizę tylko do wąskiej technologii? Na ile aplikacje Shiny umieszczone w artykułach nadal działają?
- Czy autorzy są responsywni? Czy pomagają przy problemach z kodem (e-mail, issue na GitHubie)? Czy oprogramowanie jest nadal rozwijane (np. GitHub, nowe wersje na CRAN, PyPI)?
- Czy można zaproponować postulaty reprodukowalności? Czy da się stworzyć chcecklistę, którą powinny spełniać artykuły? Jaki procent artykułów by ją spełniał? Na ile jest uniwersalna?
- Czy da się w analizach uwzględnić długość artykułu, liczbę autorów, afiliację?
- ...

Własne pomysły bardzo mile widziane.


Czasopisma, którymi można się zajmować:
    - [The R Journal](https://journal.r-project.org/)
    - [Journal of Statistical Software](https://www.jstatsoft.org/article/view/v083i11)
    - [Journal of Machine Learning Research](http://www.jmlr.org/), szczególnie część [Machine Learning Open Source Software
](http://www.jmlr.org/mloss/)
    - [Journal of Computational and Graphical Statistics](https://www.tandfonline.com/toc/ucgs20/current)
choć można i innymi :)



## Blog (15 pkt.)

Informacje w [repzytorium Wykładu](https://github.com/mini-pw/2020L-WarsztatyBadawcze)
