## TFM – Annex

Aquest repositori inclou tots els scripts i fitxers annexos utilitzats en el Treball de Fi de Màster (TFM), relacionats amb la integració i l’harmonització, el control de qualitat i l’exploració de dades de les diferents cohorts.

---

### Estructura del repositori

1. **Scripts de processament per cohort**  
   Tots els scripts que fusionen i seleccionen les dades de cada cohort es troben dins la carpeta `scripts/cohorts/`.  
   Per a cada cohort (A4, ADNI, ALFA+, OASIS3 i WRAP) hi ha:
   - `<COHORT>_merged.R`  
     Script que fusiona totes les dades en un únic dataset per a cada cohort.
   - `<COHORT>_clean.R`  
     Script de selecció de les dades d’interès per a l’anàlisi.  
   En el cas de **WRAP**, per tractar els diferents conjunts de dades (PET, MRI, atrophy, …), hi ha subcarpetes específiques amb els corresponents scripts de fusió i neteja.

2. **Unificació de totes les cohorts**  
   - `Union.R`  
     Agafa els datasets ja netejats de cada cohort i els combina en un únic conjunt de dades.

3. **Aplicació interactiva amb Shiny**  
   - `app.R`  
     Interfície Shiny per a la consulta de dades.

4. **Control de qualitat**  
   Tota la feina de control de qualitat està dins la carpeta `qc/`:
   - `qc_report.Rmd`  
     Document R Markdown on s’explica i s’executa el procés de control de qualitat.  
   - `qc_report.html`  
     Versió renderitzada en HTML del control de qualitat.

