

series <- c("pib",
            "cf","cf_s13","cf_s14",
            "fbcf","fbcf_s13","fbcf_s11","fbcf_s14",
            "m","x",
            "ipc_tot")

labels <- c("PIB",
            "Consommation","..... publiques", "..... ménage",
            "FBCF","..... publique",".... entreprises", "..... ménages",
            "Importations","Exportations","IPC")



df_tableau <- prev[,-1] %>%
  lapply(function(x)vt(x)*100) %>%
  as.data.frame() %>%
  .[(nrow(prev)-4*4+1):nrow(prev),series] %>%
  t() %>%
  as.data.frame() %>%
  round(digits=1) %>%
  format(decimal.mark = ",",
         digits = 1)

rownames(df_tableau) <- labels
df_tableau <- rownames_to_column(df_tableau)  

header_annee <-
  c("", year(prev$date[(nrow(prev) - 4 * 4 + 1):nrow(prev)])) %>% as.list()
names(header_annee) <- colnames(df_tableau)  

header_trimestre <- NULL
for (i in ((nrow(dt_pf) - 4 * 4):nrow(prev)))
  header_trimestre[i - (nrow(prev) - 4 * 4) + 1] <-
  tr(prev$date[i]) 
header_trimestre[1] <- "" 
header_trimestre%>% as.list()
names(header_trimestre) <- colnames(df_tableau)  

df_tableau %>%
  flextable() %>%
  set_header_labels(values=header_trimestre) %>%
  add_header(values=header_annee) %>%
  merge_h(part="header") %>%
  theme_zebra() %>%
  align(align="center",j=2:ncol(df_tableau),part="all")

