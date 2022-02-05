# Let's face it

Supplemental code for Thome, I., García Alanis, J.C., Volk, J., Vogelbacher, C., Steinsträter, O. &amp; Jansen, A. (2022),  Let’s face it: The lateralization of the face perception network as measured with fMRI is not clearly right dominant.

## How to use the analysis script

The data `LI_results_indiv-maxima_ROI-10mm_tthreshold_001unc.csv` (check the preprint for more information) and the analysis script `analysis_script_lets_facte_it.R` should be stored in the same directory.

Running the script should output all figures and supplementary tables containing the results reported in the manuscript.

The script takes care of installing and loading the necessary packages for it to work.

```{r}
R version 4.1.2 (2021-11-01)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)

Matrix products: default

locale:
[1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
[5] LC_TIME=German_Germany.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] effectsize_0.5    gt_0.3.1          performance_0.8.0 car_3.0-12        carData_3.0-4     lme4_1.1-27.1     Matrix_1.4-0      sjPlot_2.8.10    
 [9] Hmisc_4.6-0       Formula_1.2-4     survival_3.2-13   lattice_0.20-45   see_0.6.8         ggbeeswarm_0.6.0  viridis_0.6.2     viridisLite_0.4.0
[17] ggplot2_3.3.5     tidyr_1.1.4       dplyr_1.0.7      

loaded via a namespace (and not attached):
 [1] nlme_3.1-153        insight_0.14.5      RColorBrewer_1.1-2  tools_4.1.2         backports_1.3.0     utf8_1.2.2          R6_2.5.1           
 [8] sjlabelled_1.1.8    rpart_4.1-15        vipor_0.4.5         DBI_1.1.2           colorspace_2.0-2    nnet_7.3-16         withr_2.4.3        
[15] tidyselect_1.1.1    gridExtra_2.3       emmeans_1.7.1-1     compiler_4.1.2      htmlTable_2.3.0     bayestestR_0.11.5   scales_1.1.1       
[22] checkmate_2.0.0     mvtnorm_1.1-3       stringr_1.4.0       digest_0.6.29       foreign_0.8-81      minqa_1.2.4         base64enc_0.1-3    
[29] jpeg_0.1-9          pkgconfig_2.0.3     htmltools_0.5.2     fastmap_1.1.0       htmlwidgets_1.5.4   rlang_0.4.12        rstudioapi_0.13    
[36] farver_2.1.0        generics_0.1.1      magrittr_2.0.1      parameters_0.15.0   Rcpp_1.0.7          munsell_0.5.0       fansi_0.5.0        
[43] abind_1.4-5         lifecycle_1.0.1     stringi_1.7.6       MASS_7.3-54         grid_4.1.2          sjmisc_2.8.9        crayon_1.4.2       
[50] ggeffects_1.1.1     splines_4.1.2       sjstats_0.18.1      knitr_1.36          pillar_1.6.4        boot_1.3-28         estimability_1.3   
[57] glue_1.5.1          latticeExtra_0.6-29 modelr_0.1.8        data.table_1.14.2   nloptr_1.2.2.3      png_0.1-7           vctrs_0.3.8        
[64] tweenr_1.0.2        gtable_0.3.0        purrr_0.3.4         polyclip_1.10-0     assertthat_0.2.1    datawizard_0.2.2    xfun_0.29          
[71] ggforce_0.3.3       xtable_1.8-4        broom_0.7.10        coda_0.19-4         tibble_3.1.6        beeswarm_0.4.0      cluster_2.1.2      
[78] ellipsis_0.3.2 
```
