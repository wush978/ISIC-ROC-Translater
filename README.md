# 中華民國行業標準分類--各版本對應表

## 簡介

[中華民國行業標準分類](http://www.dgbas.gov.tw/lp.asp?CtNode=3111&CtUnit=566&BaseDSD=7&mp=1)中有各次修訂的版本，本專案以電腦可讀的方式提供各版本間的對應表。

## 檔案目錄

- 6to7.txt 將第六次版本的細類對應至第七次版本的細類
- 7to8.txt 將第七次版本的細類對應至第八次版本的細類
- 8to9.txt 將第八次版本的細類對應至第九次版本的細類

## 檔案格式

以下以`6to7.txt`為例，解釋檔案資料格式

```
0131	0121
...
0123	0134,9009
...
5264	NA
...
```

- 第六版細類`0131`對應到第七版細類`0121`
- 第六版細類`0123`對應到第七版細類`0134,9009`
- 第六版細類`5264`消失了


## LICENSE

## 使用範例

### R 語言

```r
mapping <- list()
for(name in c("6to7", "7to8", "8to9")) {
  mapping[[name]] <- readLines(sprintf("SICS(ROC)/%s.txt", name))
}

parse_mapping <- function(x) {
  regmatches(x, regexec("^(\\d+)\\t([0-9,]+)", x)) %>%
    lapply(function(s) {
      data.frame(from = s[2], to = strsplit(s[3], ",")[[1]], stringsAsFactors = FALSE)
    })  %>%
    do.call(what = rbind) %>%
    mutate(from_label = substring(from, 1, 2), to_label = substring(to, 1, 2)) %>%
    select(from_label, to_label)
}
mapping <- lapply(mapping, parse_mapping)

map <- function(src, mapping) {
  src <- filter(src, grepl("^\\d{2}$", label))
  dst <- lapply(seq_len(nrow(src)), function(i) {
    tryCatch({
      tmp <- filter(mapping, from_label == src$label[i])
      if (nrow(tmp) == 0) data.frame() else { tmp %>%
        group_by(to_label) %>%
        summarise(size = length(to_label)) %>%
        mutate(value = src$value[i] * size / sum(size)) %>%
        select(to_label, value) %>%
        mutate(label = to_label, year = src$year[i])
        as.data.frame
      }},
      error = function(e) browser())
  }) %>%
    do.call(what = rbind) %>%
    group_by(to_label) %>%
    summarise(value = sum(value)) %>%
    mutate(label = to_label)
    as.data.frame
  dst
}
```



