---
title: "Statexpres's: Rapid Development with Mentalab Explore - Advanced Signal Exploration in Sleep EEG (statexpres.com/sleep)"
output:
  flexdashboard::flex_dashboard:
    storyboard: TRUE
    always_allow_html: yes
  pdf_document:
    classoption: landscape
    documentclass: ar-1col
    header-includes: \usepackage{fancyhdr,tabu, courier,chngcntr,longtable, graphicx,float,natbib,geometry}
    keep_tex: TRUE
    fig_caption: yes
    number_sections: yes
    always_allow_html: yes
runtime: shiny
---

<style>
  .storyboard-nav .sbframelist {
        margin: 0 auto;
        width: 94%;
        height: 90px;
        overflow: hidden;
        text-shadow: none;
        margin-bottom: 3px;
        margin-top: 3px;
  }
  .storyboard-nav .sbnext, .storyboard-nav .sbprev {
        float: left;
        width: 20px;
        height: 20px;
        font-size: 20px;
  }
</style>








<!-- \lhead{MentaLab's Signal Evaluation \linebreak {\leftmark}  } -->

\clearpage
  
<!-- ### Signal Source -->


<!-- \begin{tcolorbox}[colback=lhi!30,%gray background -->
<!--                 colframe=lhi!60,% black frame colour -->
<!--                 arc=1mm, auto outer arc,] -->
<!-- MentaLab's Signal Evaluation employes recent signal processing tools, to evaluate ECG and EEG data.     -->
<!-- Furthermore it holds relevant citation and customizable output formats.     -->
<!-- It is extandable by Python and R data analysis methods and curated by professional biostatisticians and medical experts.     -->
<!-- \end{tcolorbox} -->

### Input - data is assumed to be in Voltage and will be converted based on biosource, initial rounding to one microvolt.
<!-- {.sidebar} -->







### Source & Filter - add edges from channels to biosources, select biosources and apply filter settings












***










### Sleep Spindles and QRS Detection

NULL


***








### Singular Spectrum Analysis - Manual Clustering: Click on Component (red square) -> Click on Component in New Cluster













### Data Viewer & PDF - data is downsampled based on window range (a) more than 30 minutes (b) between 30 minutes and 20 seconds (c) less than 20 seconds

NULL








