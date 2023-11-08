---
title: "electoral_report"
author: "Victor Batista"
format: html
editor: visual
---

```{r}
#| label: libraries
#| include: false

library(readr)
library(dplyr)
library(magrittr)
library(knitr)
library(ggplot2)
library(scales)
library(forcats)


knit_hooks$set(inline = function(x) {
  prettyNum(x,
            big.mark = ",",
            format="f",
            digits=4)
})
```

```{r}
#| label: input-data
#| include: false
#| cache: true

dropbox <- "C:/Users/victo/dropbox/EEEDataLabs-Brazil"
input <- file.path(dropbox, "CLEAN_DATA/ELECTORAL_DATA")
  
justification <- readRDS(file.path(input, "JUSTIFICATION/justificativa.RDS"))
registration <- readRDS(file.path(input, "REGISTRATION/registration.RDS"))
turnout <- readRDS(file.path(input, "TURNOUT/turnout_2022.RDS"))
turnout_tte <- readRDS(file.path(input, "TURNOUT/turnout_tte_2022.RDS"))
population <- readRDS(file.path(input, "populationBR_year.RDS"))

```

```{r}
#| label: somecalculations
#| include: false
#| cache: true

populacao <- population %>% filter(ano == 2021) %$% populacao
```

## Introduction

### Population of brazil

According to the Continuous PNAD, a sample survey carried out annually by the IBGE, the Brazilian population in 2021 was `r populacao` people.

## Turnout

```{r}
#| label: turnout
#| include: false
#| cache: true

comparecimento_1t <- turnout |> 
  filter(NR_TURNO == 1) |> 
  select(QT_COMPARECIMENTO)

comparecimento_1t <- sum(comparecimento_1t$QT_COMPARECIMENTO)

comparecimento_2t <- turnout |> 
  filter(NR_TURNO == 2) |> 
  select(QT_COMPARECIMENTO)

comparecimento_2t <- sum(comparecimento_2t$QT_COMPARECIMENTO)

abstencao_1t <- turnout |> 
  filter(NR_TURNO == 1) |> 
  select(QT_ABSTENCAO)

abstencao_1t <- sum(abstencao_1t$QT_ABSTENCAO)

abstencao_2t <- turnout |> 
  filter(NR_TURNO == 2) |> 
  select(QT_ABSTENCAO)

abstencao_2t <- sum(abstencao_2t$QT_ABSTENCAO)

```

### Number of registered voters

`r comparecimento_1t + abstencao_2t` people were registered to vote in 2022

### Number of people who voted

The turnout in the first round is `r comparecimento_1t/(comparecimento_1t + abstencao_1t)*100`% and `r comparecimento_2t/(comparecimento_2t + abstencao_2t)*100`% in the second

## Justification, transit and registration

### Number of justifications

```{r}
#| label: justifications
#| include: false
#| cache: true

justificativa_1t <- justification |> 
  filter(Turno == 1) |> 
  select(`Quantidade de justificativas`)

justificativa_1t <- sum(justificativa_1t$`Quantidade de justificativas`)

justificativa_2t <- justification |> 
  filter(Turno == 2) |> 
  select(`Quantidade de justificativas`)

justificativa_2t <- sum(justificativa_2t$`Quantidade de justificativas`)

```

`r justificativa_1t/(comparecimento_1t + abstencao_1t)*100`% of the voters justificated their presence in the first round of 2022 elections, while `r justificativa_2t/(comparecimento_1t + abstencao_1t)*100`% did this in the second round.

```{r}
#| label: just_graph1
#| echo: false
#| cache: true

justification |>
  group_by(UF) |> 
  summarize(contagem = sum(`Quantidade de justificativas`)) |> 
  ggplot(aes(x=fct_reorder(UF, contagem), y=contagem, color=UF)) +
  geom_col()+
  labs (
    title = "Justifications, by state",
    x="State",
    y="Number of processes")+
  theme(legend.position = "none")
```

```{r}
#| label: just_graph2
#| echo: false
#| cache: true
#| warning: false

justification |> 
  mutate(inside_state = UF ==`UF justificativa`) |> 
  ggplot(aes(x=inside_state))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels = percent)+
  labs (
    title = "Amount of justifications inside the same state",
    x="Was the process inside the state?",
    y="Number of processes")+
  theme(legend.position = "none")
```

### Number of transit

```{r}
#| label: transit-calculations
#| echo: false
#| cache: true

turno1 <- turnout_tte |> 
  filter(NR_TURNO==1) |> 
  filter(CD_TIPO_TRANSFERENCIA == 1)

turno2 <- turnout_tte |> 
  filter(NR_TURNO==2) |> 
  filter(CD_TIPO_TRANSFERENCIA == 1)

compar1t <- sum(turno1$QT_COMPARECIMENTO_TTE)
compar2t <- sum(turno2$QT_COMPARECIMENTO_TTE)
abst1t <- sum(turno1$QT_ABSTENCAO_TTE)
abst2t <- sum(turno2$QT_ABSTENCAO_TTE)

```

In the first round, `r compar1t+abst1t` were able to vote in transit. Of these, `r abst1t/(compar1t+abst1t)*100`% abstained on voting day. In the second round, `r compar2t+abst2t` were able to vote in transit. Of these, `r abst2t/(compar2t+abst2t)*100`% abstained on voting day

These are top 10 cities that were ORIGIN of transit votes

```{r}
#| label: top-10-origins
#| echo: false
#| cache: true

turnout_tte |> 
  filter(CD_TIPO_TRANSFERENCIA == 1) |> 
  select(NM_MUNICIPIO_ORIGEM, QT_COMPARECIMENTO_TTE) |> 
  group_by(NM_MUNICIPIO_ORIGEM) |> 
  summarize(number_of_people=sum(QT_COMPARECIMENTO_TTE)) |> 
  arrange(desc(number_of_people))

```

And these are top 10 cities that were DESTINATION of transit votes

```{r}
#| label: top-10-destinations
#| echo: false
#| cache: true

turnout_tte |>
  filter(CD_TIPO_TRANSFERENCIA == 1) |> 
  select(NM_MUNICIPIO_DESTINO, QT_COMPARECIMENTO_TTE) |> 
  group_by(NM_MUNICIPIO_DESTINO) |> 
  summarize(number_of_people=sum(QT_COMPARECIMENTO_TTE)) |> 
  arrange(desc(number_of_people))
```

### Number of switched registrations

```{r}
#| label: register-calculations
#| echo: false
#| cache: true

rae2022 <- registration |> 
  filter(`Tipo de operação` == "Transferência") |> 
  select(`Quantidade de RAE`)

rae2022 <- sum(rae2022$`Quantidade de RAE`)
```

`r rae2022` processes of switch registration were done in Brazil on the last 12 months before election.

```{r}
#| label: graph3
#| echo: false
#| cache: true
registration |> 
  filter(`Tipo de operação` == "Transferência") |> 
  group_by(UF) |> 
  summarize(contagem = sum(`Quantidade de RAE`)) |> 
  ggplot(aes(x=fct_reorder(UF, contagem), y=contagem, color=UF)) +
  geom_col()+
  labs (
    title = "Registration transfers, by state",
    x="State",
    y="Number of processes")+
  theme(legend.position = "none")
```
