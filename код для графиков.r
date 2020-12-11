library("corrplot", warn.conflicts = FALSE )
library("car", warn.conflicts = FALSE )
library("ggplot2", warn.conflicts = FALSE )
library("stargazer", warn.conflicts = FALSE )
library("magrittr", warn.conflicts = FALSE )
library("knitr", warn.conflicts = FALSE )
library("dplyr", warn.conflicts = FALSE )
library("tidyr", warn.conflicts = FALSE )
library("lmtest", warn.conflicts = FALSE )
library("olsrr", warn.conflicts = FALSE)
library("sandwich", warn.conflicts = FALSE )
library("Matching", warn.conflicts = FALSE )
library("tableone", warn.conflicts = FALSE )
library("kableExtra", warn.conflicts = FALSE )
library("xtable", warn.conflicts = FALSE )
library("magick", warn.conflicts = FALSE )
library("glmnet", warn.conflicts = FALSE )
library("grf", warn.conflicts = FALSE )
library("randomForest", warn.conflicts = FALSE )
library("pwt9", warn.conflicts = FALSE )
library("readxl", warn.conflicts = FALSE )
library("foreign", warn.conflicts = FALSE )
library("Synth", warn.conflicts = FALSE )
library("gridExtra", warn.conflicts = FALSE )



Data = read.csv("C://Users/Kate/Downloads/CIAN.csv", encoding = "UTF-8")[,-c(1,8,15)]
levels(Data$РљРѕРјРЅР°С‚)=c("1-РєРѕРјРЅ.","2-РєРѕРјРЅ.","3-РєРѕРјРЅ.","4-РєРѕРјРЅ.","5-РєРѕРјРЅ.","РђРїР°СЂС‚.СЃРІРѕР±.РїР»Р°РЅРёСЂРѕРІРєРё", "РљРІ.СЃРІРѕР±.РїР»Р°РЅРёСЂРѕРІРєРё", "РњРЅРѕРіРѕРєРѕРјРЅР°С‚РЅР°СЏ", "РЎС‚СѓРґРёСЏ")
levels(Data$Р Р°Р№РѕРЅ)[5] = "СЂ-РЅ РќРѕРІРѕ-РЎР°РІРёРЅРѕРІСЃРєРёР№"



data_frame("РџРµСЂРµРјРµРЅРЅР°СЏ" = names(Data),
           "РљР»Р°СЃСЃ" = sapply(Data, class),
           "РџСЂРёРјРµСЂ Р·РЅР°С‡РµРЅРёР№" = sapply(Data, function(x) paste0(x[20:30],  collapse = "; ")),
           row.names = NULL) %>% 
  kable(format = "latex", longtable = T) %>%
  column_spec(2, width = "4em") %>%
  column_spec(3, width = "25em")




CORR = cor(Data[,c(1:4,10)],use = "na.or.complete")
corrplot(CORR, type = "lower", tl.col = "black", tl.srt = 37, cl.cex = 0.55, tl.cex = 0.8, diag = F, order="FPC")
corrplot(CORR, type = "lower", tl.col = "black", tl.srt = 37, cl.cex = 0.55, tl.cex = 0.8, diag = F, order="FPC", method ="number")



ggplot(Data, aes(x = Р¦РµРЅР°, fill = РљРѕРјРЅР°С‚)) + 
  geom_density(alpha=.6) +
  theme(text = element_text(size=30)) + 
  labs(title="РџР»РѕС‚РЅРѕСЃС‚СЊ СЂР°СЃРїСЂРµРґРµР»РµРЅРёСЏ С†РµРЅ РїРѕ РєРѕР»РёС‡РµСЃС‚РІСѓ РєРѕРјРЅР°С‚", fill="РљРѕРјРЅР°С‚РЅРѕСЃС‚СЊ" )

ggplot(filter(Data, Р¦РµРЅР° > 10000000), aes(x = Р¦РµРЅР°)) + 
  geom_density() +
  theme(text = element_text(size=30)) + 
  labs(title="РџР»РѕС‚РЅРѕСЃС‚СЊ СЂР°СЃРїСЂРµРґРµР»РµРЅРёСЏ С†РµРЅ" )



ggplot(Data, aes(x = Р¦РµРЅР°, fill = РўРёРї.Р¶РёР»СЊСЏ)) + 
  geom_density(alpha=.6) +
  theme(text = element_text(size=30)) + 
  labs(title="РџР»РѕС‚РЅРѕСЃС‚СЊ СЂР°СЃРїСЂРµРґРµР»РµРЅРёСЏ С†РµРЅ РїРѕ С‚РёРїСѓ Р¶РёР»СЊСЏ", fill="РўРёРї Р¶РёР»СЊСЏ" )


ggplot(Data, aes(x = Р¦РµРЅР°, fill = Р Р°Р№РѕРЅ)) + 
  geom_density(alpha=.6) +
  theme(text = element_text(size=30)) + 
  labs(title="РџР»РѕС‚РЅРѕСЃС‚СЊ СЂР°СЃРїСЂРµРґРµР»РµРЅРёСЏ С†РµРЅ РїРѕ СЂР°Р№РѕРЅР°Рј", fill="Р Р°Р№РѕРЅ" )


ggplot(Data, aes(x = Р¦РµРЅР°, fill = РћС‚РґРµР»РєР°)) + 
  geom_density(alpha=.6) +
  theme(text = element_text(size=30)) + 
  labs(title="РџР»РѕС‚РЅРѕСЃС‚СЊ СЂР°СЃРїСЂРµРґРµР»РµРЅРёСЏ С†РµРЅ РїРѕ РѕС‚РґРµР»РєРµ", fill="РћС‚РґРµР»РєР°" )


ggplot(Data, aes(x = Р¦РµРЅР°, fill = Р РµРјРѕРЅС‚)) + 
  geom_density(alpha=.6) +
  theme(text = element_text(size=30)) + 
  labs(title="РџР»РѕС‚РЅРѕСЃС‚СЊ СЂР°СЃРїСЂРµРґРµР»РµРЅРёСЏ С†РµРЅ РїРѕ СЂРµРјРѕРЅС‚Сѓ", fill="Р РµРјРѕРЅС‚" )


ggplot(Data, aes(x = РћР±С‰Р°СЏ, y = Р¦РµРЅР°, color = factor(РљРѕРјРЅР°С‚))) +
  geom_point() +
  theme(text = element_text(size=20)) + 
  labs(title="Р“СЂР°С„РёРє СЂР°Р·Р±СЂРѕСЃР° С†РµРЅ РѕС‚ РѕР±С‰РµР№ РїР»РѕС‰Р°РґРё", color="РљРѕРјРЅР°С‚РЅРѕСЃС‚СЊ", x="РћР±С‰Р°СЏ РїР»РѕС‰Р°РґСЊ, Рј2") +
  geom_smooth(method = lm)

ggplot(Data, aes(x = РћР±С‰Р°СЏ, y = Р¦РµРЅР°, color = Р Р°Р№РѕРЅ)) +
  geom_point() +
  theme(text = element_text(size=20)) + 
  labs(title="Р“СЂР°С„РёРє СЂР°Р·Р±СЂРѕСЃР° С†РµРЅ РѕС‚ РѕР±С‰РµР№ РїР»РѕС‰Р°РґРё", color="Р Р°Р№РѕРЅ", x="РћР±С‰Р°СЏ РїР»РѕС‰Р°РґСЊ, Рј2") +
  geom_smooth(method = lm)

ggplot(Data, aes(x=Р Р°Р№РѕРЅ, y=Р¦РµРЅР°, color = Р Р°Р№РѕРЅ)) +
  geom_boxplot() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=17,hjust = 0.85),
        legend.position = "none")

ggplot(Data, aes(x=Р Р°Р№РѕРЅ, y=РћР±С‰Р°СЏ, color = Р Р°Р№РѕРЅ)) +
  geom_boxplot() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=17,hjust = 0.85),
        legend.position = "none") +
  labs(y = "РћР±С‰Р°СЏ РїР»РѕС‰Р°РґСЊ, Рј2")



ggplot(Data, aes(x=as.factor(Р­С‚Р°Р¶), y=Р¦РµРЅР°, color = as.factor(Р­С‚Р°Р¶))) +
  geom_boxplot() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(hjust = 0.85),
        legend.position = "none") +
  labs(x = "Р­С‚Р°Р¶")


ggplot(Data, aes(x=РўРёРї.Р¶РёР»СЊСЏ, y=Р¦РµРЅР°, color = Р РµРјРѕРЅС‚)) +
  geom_boxplot() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(hjust = 0.85,angle=17)) +
  labs(x = "РўРёРї Р¶РёР»СЊСЏ")

ggplot(Data, aes(x=РљРѕРјРЅР°С‚, y=Р¦РµРЅР°, color = РљРѕРјРЅР°С‚)) +
  geom_boxplot() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(hjust = 0.85,angle=17),
        legend.position = "none") +
  labs(x = "РљРѕР»РёС‡РµСЃС‚РІРѕ РєРѕРјРЅР°С‚")





reg1 = lm(Data, formula = log(Р¦РµРЅР°) ~ log(РћР±С‰Р°СЏ) + log(Р–РёР»Р°СЏ) + log(РљСѓС…РЅСЏ) + log(Р­С‚Р°Р¶) + РљРѕРјРЅР°С‚ + Р Р°Р№РѕРЅ + РџРѕСЃС‚СЂРѕРµРЅ + Р РµРјРѕРЅС‚ + РЎР°РЅСѓР·РµР» + Р‘Р°Р»РєРѕРЅ.Р»РѕРґР¶РёСЏ + РўРёРї.Р¶РёР»СЊСЏ)

reg1 = lm(Data, formula = log(Р¦РµРЅР°) ~ log(РћР±С‰Р°СЏ) + log(Р–РёР»Р°СЏ) + log(РљСѓС…РЅСЏ) + log(Р­С‚Р°Р¶))
reg2 = lm(Data, formula = log(Р¦РµРЅР°) ~ log(РћР±С‰Р°СЏ) + log(Р–РёР»Р°СЏ) + log(РљСѓС…РЅСЏ) + log(Р­С‚Р°Р¶) + РљРѕРјРЅР°С‚ + Р Р°Р№РѕРЅ + РџРѕСЃС‚СЂРѕРµРЅ)
reg3 = lm(Data, formula = log(Р¦РµРЅР°) ~ log(РћР±С‰Р°СЏ) + log(Р–РёР»Р°СЏ) + log(РљСѓС…РЅСЏ) + log(Р­С‚Р°Р¶) + Р РµРјРѕРЅС‚ + РЎР°РЅСѓР·РµР» + Р‘Р°Р»РєРѕРЅ.Р»РѕРґР¶РёСЏ)




stargazer(reg1,reg2,reg3, font.size="footnotesize", header=FALSE, no.space=TRUE, single.row=TRUE, column.labels = c("РњРѕРґРµР»СЊ 1", "РњРѕРґРµР»СЊ 2", " РњРѕРґРµР»СЊ 3"), column.sep.width = "-5pt", table.placement = "H")



ols_plot_resid_lev(reg1)
ols_plot_resid_stud_fit(reg1)
p = ols_plot_resid_lev(reg1)
c = pull(p$leverage[,1])
reg1 = lm(Data[-c, ], formula = log(Р¦РµРЅР°) ~ log(РћР±С‰Р°СЏ) + log(Р–РёР»Р°СЏ) + log(РљСѓС…РЅСЏ) + log(Р­С‚Р°Р¶))
ols_plot_resid_lev(reg1)
ols_plot_resid_stud_fit(reg1)

high.value <- which(Data$Р¦РµРЅР° > 10000000) 
DataNEW <- Data[-high.value, ]
reg1 = lm(Data, formula = log(Р¦РµРЅР°) ~ log(РћР±С‰Р°СЏ) + log(Р–РёР»Р°СЏ) + log(РљСѓС…РЅСЏ) + log(Р­С‚Р°Р¶))
ols_plot_resid_lev(reg1)
ols_plot_resid_stud_fit(reg1)


qnt <- quantile(x$Р¦РµРЅР°, probs=c(.25, .75), na.rm = T)
H <- 1.5 * IQR(x$Р¦РµРЅР°, na.rm = T)
y[x$Р¦РµРЅР° > (qnt[2] + H),] <- NA
y



```