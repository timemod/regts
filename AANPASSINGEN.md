# Verklaring van de Aanpassingen in `regts`

Hieronder volgt een overzicht en verklaring van de wijzigingen die zijn doorgevoerd in de `regts` repository.

## 1. `pkg/src/Makevars`
- **Wijziging**: De regel `PKG_CXXFLAGS = $(R_HOME)/bin/Rscript -e "Rcpp:::CxxFlags()"` is verwijderd.
- **Verklaring**: Omdat het `DESCRIPTION` bestand van het pakket `LinkingTo: Rcpp` bevat, regelt R automatisch de juiste include-paden voor Rcpp tijdens de compilatie. Het expliciet aanroepen van `Rcpp:::CxxFlags()` in `Makevars` is hierdoor overbodig geworden en kan soms zelfs voor dubbele vlaggen zorgen.

## 2. `Makefile`
- **Wijziging**: 
    - `PKG_CXXFLAGS` is vervangen door `RCPP_INC`.
    - De manier waarop het Rcpp include-pad wordt opgehaald is veranderd naar `Rscript -e 'cat(system.file("include", package="Rcpp"))'`.
    - Het `syntax` target gebruikt nu `-I$(RCPP_INC)` in plaats van de volledige `PKG_CXXFLAGS`.
- **Verklaring**: Deze wijziging maakt het `Makefile` robuuster voor syntax-checking. Door specifiek het include-pad te isoleren, kan de syntax-check (`make syntax`) gerichter worden uitgevoerd zonder onnodige compiler-vlaggen die alleen bij de daadwerkelijke build van belang zijn.

## 3. `install_deps.R`
- **Wijziging**: `devtools` is vervangen door `pak`.
- **Verklaring**: 
    - `pak` is een moderne, snellere en betrouwbaardere package manager voor R.
    - `pak::local_install_deps("pkg", upgrade = FALSE)` vervangt de `devtools::install_deps` aanroep. Dit zorgt voor een efficiëntere installatie van afhankelijkheden, waarbij netwerkverkeer en bouwtijd vaak worden gereduceerd door beter dependency management.

---

Deze aanpassingen zorgen voor een schonere build-configuratie en een efficiënter ontwikkelproces.
