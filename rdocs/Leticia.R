library(tidyverse)
library(readr)
banco <- read_csv("StudentPerformanceFactors.csv")


estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
           "#663333", "#FF6600", "#CC9966",
           "#999966", "#006606", "#008091", 
           "#041835", "#666666" )
           
theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

print_quadro_resumo <- function(data, var_name, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  var_name <- substitute(var_name)
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs = .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5),2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs = .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n", sep="")
  }
  
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
}

## mexendo no banco 
banco <- banco %>%
  rename(`Horas de Sono` = Sleep_Hours, `Nota do Exame` = Exam_Score, `Horas de Estudo por Semana` = Hours_Studied, 
         `Nível de Motivação` = Motivation_Level, Frequência = Attendance, Sexo = Gender, `Notas Anteriores` = Previous_Scores, `Atividade Física`= Physical_Activity, 
         `Educação dos Pais` = Parental_Education_Level)
banco$`Nível de Motivação` <- as.factor(banco$`Nível de Motivação`)
levels(banco$`Nível de Motivação`)
levels(banco$`Nível de Motivação`) <- c("Alto", "Baixo", "Médio")

banco$Sexo <- as.factor(banco$Sexo)
levels(banco$Sexo)
levels(banco$Sexo) <- c("Feminino", "Masculino")

banco$`Educação dos Pais` <- as.factor(banco$`Educação dos Pais`)
levels(banco$`Educação dos Pais`)
levels(banco$`Educação dos Pais`) <- c("Graduação Completa", "Ensino Médio Completo", "Pós-Graduação Completa")
##### Horas de Estudo por Semana X Nota no Exame final 

## Análise errada
ggplot(banco) +
  aes(x = `Nota do Exame`, y = `Horas de Estudo por Semana`) + 
  geom_line(colour = "blue", size = 3) + 
  labs(
    x = "Nota do Exame",  
    y = "Horas de Estudo por Semana"
  ) +
  theme_minimal(base_size = 14)  
ggsave("análise1ERRADA.pdf", width = 158, height = 93, units = "mm")

## Análise certa
ggplot(banco) +
  aes(x = `Horas de Estudo por Semana`, y = `Nota do Exame`) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Horas de Estudo por Semana",
    y = "Nota do Exame"
  ) +
  theme_estat()

ggsave("análise1CERTA.pdf", width = 158, height = 93, units = "mm")


banco %>% 
  print_quadro_resumo(var_name = `Horas de Estudo por Semana`)

banco %>% 
  print_quadro_resumo(var_name = `Nota do Exame`)
##### Horas de Sono X Nível de motivação
## Análise errada
ggplot(banco) +
  aes(x = reorder(`Nível de Motivação`, `Horas de Sono`, FUN = median), y = `Horas de Sono`, fill = `Nível de Motivação`) +  # Mapeando o fill para a variável categórica
  geom_boxplot(
    colour = "yellow",  # Cor das bordas contrastante
    width = 2  # Largura exagerada
  ) +
  scale_fill_manual(values = c("#A11D21", "#1DA1F2", "#11D221", "#F1A21D", "#D211F1")) +  # Definindo manualmente cores conflitantes
  stat_summary(
    fun = "mean", 
    geom = "point", 
    shape = 21, 
    size = 10,  # Tamanho muito grande dos pontos
    fill = "pink"  # Cor dos pontos conflitante
  ) +
  labs(
    x = "Motivação vs Sono",  # Título exagerado e confuso
    y = "Número de Horas Dormidas por Dia"  # Texto desnecessariamente longo
  )
ggsave("análise2ERRADA.pdf", width = 158, height = 93, units = "mm")

    

## Análise certa
ggplot(banco) +
  aes(x = reorder(`Nível de Motivação`, `Horas de Sono`, FUN = median), y = `Horas de Sono`) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Nível de Motivação", y = "Horas de Sono") +
  theme_estat()
ggsave("análise2CERTA.pdf", width = 158, height = 93, units = "mm")

banco %>% 
  group_by(`Nível de Motivação`) %>% 
  print_quadro_resumo(var_name = `Horas de Sono`)

##### Frequência nas aulas X Sexo 

## Análise errada
ggplot(banco) +
  aes(x = reorder(Sexo, Frequência, FUN = median), y = Frequência, fill = Sexo) + 
  geom_boxplot(
    colour = "cyan",  
    width = 1.5  
  ) +
  stat_summary(
    fun = "mean", 
    geom = "point", 
    shape = 18, 
    size = 7,  
    fill = "purple"
  ) +
  labs(
    x = "Sexo e Motivação",  
    y = "Frequência das Horas de Sono Diárias", 
  ) +
  scale_fill_manual(values = c("hotpink", "lightblue"))  
ggsave("análise3ERRADA.pdf", width = 158, height = 93, units = "mm")


## Análise certa
ggplot(banco) +
  aes(x = reorder(Sexo, Frequência, FUN = median), y = Frequência) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Gênero", y = "Frequência nas aulas") +
  theme_estat()
ggsave("análise3CERTA.pdf", width = 158, height = 93, units = "mm")


banco %>% 
  group_by(Sexo) %>% 
  print_quadro_resumo(var_name = Frequência)

##### Escolaridade dos pais 
## Análise errada

escolaridade <- banco %>%
  filter(!is.na(`Educação dos Pais`)) %>%
  count(`Educação dos Pais`) %>%
  mutate(
    freq = n,
    relative_freq = round((freq / sum(freq)) * 100, 1),
    freq = gsub("\\.", ",", relative_freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(escolaridade) +
  aes(x = fct_reorder(`Educação dos Pais`, n, .desc = TRUE), y = n, label = label) +
  geom_bar(stat = "identity", fill = "hotpink", width = 1.2, alpha = 0.8) + 
  geom_text(
    position = position_stack(vjust = 0.5),  
    size = 6,  
    angle = 45,  
    color = "cyan"  
  ) +
  labs(
    x = "Nível de Escolaridade dos Pais", 
    y = "Número Absoluto e Relativo"
  )
ggsave("análise4ERRADA.pdf", width = 158, height = 93, units = "mm")


## Análise certa
escolaridade <- banco %>%
  filter(!is.na(`Educação dos Pais`)) %>%
  count(`Educação dos Pais`) %>%
  mutate(
    freq = n,
    relative_freq = round((freq / sum(freq)) * 100, 1),
    freq = gsub("\\.", ",", relative_freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(escolaridade) +
  aes(x = fct_reorder(`Educação dos Pais`, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Escolaridade dos Pais", y = "Frequência") +
  theme_estat()
ggsave("análise4CERTA.pdf", width = 158, height = 93, units = "mm")
