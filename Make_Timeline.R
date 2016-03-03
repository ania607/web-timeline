# Упражнение 1.-----------------------------------------------------------------

# Загрузка пакетов
library('XML')                 
library('RCurl')               
# Создадим вектора для последующего заполнения ...  
years <- vector(mode = "numeric", length = 0) # ..годов
headers <- vector(mode = "character", length = 0) # ...заголовков
sources <- vector(mode = "character", length = 0) # ...источников информации
urls <- vector(mode = "character", length = 0) # ... рабочих ссылок
# Вектор с тремя вариантами запросов
request_names <- c("Изменение климата в России",
                   "Климат* изменение в России",
                   "Россия климат* изменение")
# Вектор URL страниц поиска соответственно трем вариантам запросов (Nigma)
request_n <- c("http://www.nigma.ru/?s=%D0%B8%D0%B7%D0%BC%D0%B5%D0%BD%D0%B5%D0%BD%D0%B8%D0%B5+%D0%BA%D0%BB%D0%B8%D0%BC%D0%B0%D1%82%D0%B0+%D0%B2+%D1%80%D0%BE%D1%81%D1%81%D0%B8%D0%B8+",
               "http://www.nigma.ru/?s=%D0%9A%D0%BB%D0%B8%D0%BC%D0%B0%D1%82*+%D0%B8%D0%B7%D0%BC%D0%B5%D0%BD%D0%B5%D0%BD%D0%B8%D0%B5+%D0%B2+%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D0%B8+",
               "http://www.nigma.ru/?s=%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D1%8F+%D0%BA%D0%BB%D0%B8%D0%BC%D0%B0%D1%82*+%D0%B8%D0%B7%D0%BC%D0%B5%D0%BD%D0%B5%D0%BD%D0%B8%D0%B5+")

# Вектор URL страниц поиска соответственно трем вариантам запросов (Яндекс)
request_y <- c("https://yandex.ru/search/?text=%D0%98%D0%B7%D0%BC%D0%B5%D0%BD%D0%B5%D0%BD%D0%B8%D0%B5%20%D0%BA%D0%BB%D0%B8%D0%BC%D0%B0%D1%82%D0%B0%20%D0%B2%20%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D0%B8%20",
               "https://yandex.ru/search/?text=%D0%9A%D0%BB%D0%B8%D0%BC%D0%B0%D1%82*%20%D0%B8%D0%B7%D0%BC%D0%B5%D0%BD%D0%B5%D0%BD%D0%B8%D0%B5%20%D0%B2%20%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D0%B8%20",
               "https://yandex.ru/search/?text=%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D1%8F%20%D0%BA%D0%BB%D0%B8%D0%BC%D0%B0%D1%82*%20%D0%B8%D0%B7%D0%BC%D0%B5%D0%BD%D0%B5%D0%BD%D0%B8%D0%B5%20")

# 1. Поиск с помощью Нигма====================================================== 
# Так как на число запросов действует ограничение, будем собирать 
# информацию за небольшое количество лет (максимум с одного запроса - 5 лет)

# Попробуем за один год - 2017.

for (i in request_n) {
  for (n in 2017:2017) {
    # URL страницы поиска в Нигма:
    fileURL <- paste(i,n,
                     "&t=web&rg=t%3D%D0%9C%D0%BE%D1%81%D0%BA%D0%B2%D0%B0_c%3D%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D1%8F_&rg_view=%D0%9C%D0%BE%D1%81%D0%BA%D0%B2%D0%B5&yn=1&gl=1&rm=1&ms=1&yh=1&av=1&nm=1&lang=all&srt=0&sf=1",
                     sep = "")
    fileURL
    # Загружаем текст страницы
    html <- getURL(fileURL)
    # разбираем как html
    doc <- htmlTreeParse(html, useInternalNodes = T)
    # корневой элемент
    rootNode <- xmlRoot(doc)
    # заполняем столбец для лет
    years <- c(years, rep(n, 10))
    # выбираем все заголовки результатов запроса статей
    headers <- c(headers, xpathSApply(rootNode, '//div[@class="snippet_title"]',
                                      xmlValue))
    # выбираем все источники результатов запроса
    sources <- c(sources, gsub('http://', '',
                               xpathSApply(rootNode, 
                                           '//div[@class="snippet_title"]/a',
                                           xmlGetAttr, 'href')))
    # заполняем столбец для ссылок
    urls <- c(urls,xpathSApply(rootNode, '//div[@class="snippet_title"]/a',
                               xmlGetAttr, 'href'))
    
  }
  
}


# собираем в один фрейм
DF.all <- data.frame(Year = years, Header = headers,
                   Source = sources, URL = urls)
# Приведем заголовки к более красивому виду - уберем лишние ссылки и ...
DF.all$Header <- gsub(' [/||] .*$', '', DF.all$Header)
DF.all$Header <- gsub('[...]', '', DF.all$Header)


# запишем результат в файл
  if (!file.exists('./TimelineN.csv')) {
    file.create('./TimelineN.csv')
  }
write.csv(DF.all, './TimelineN.csv', row.names = F)


# 2. Поиск с помощью Яндекс===================================================== 
# Посмотрим результаты на 10 лет с помощью Яндекс


# Создадим вектора для последующего заполнения ...  
years_y <- vector(mode = "numeric", length = 0) # ..годов
headers_y <- vector(mode = "character", length = 0) # ...заголовков
sources_y <- vector(mode = "character", length = 0) # ...источников информации
urls_y <- vector(mode = "character", length = 0) # ... рабочих ссылок


for (n in 2017:2026){
  # Sys.sleep(120)
  for  (i in request_y){
    # URL страницы поиска в Яндекс:
    fileURL <- paste(i, n,
                     "&lr=213&clid=2008266-300&win=122",
                     sep = "")
    
    # Загружаем текст страницы
    html <- getURL(fileURL)
    # разбираем как html
    doc <- htmlTreeParse(html, useInternalNodes = T)
    
    # Sys.sleep(40)
    
    # корневой элемент
    rootNode <- xmlRoot(doc)
    # Теперь следует избавиться от записей, предоставленных "Яндекс Новости", а
    # также Яндекс Видео
    # Эти записи создают путаницу, так как под одним заголовком может быть 
    # несколько новостей, и из-за этого получается разное число ссылок и 
    # источников.
    
    # Примечание: тег заголовков основной - первый, остальные (//div[@class="z-video__item"]/a
    # |  //div[@class="z-video__item z-video__item_big-screen_yes"]/a) добавлены
    # для того, чтобы учесть, если в результате поиска появятся видео-материалы
    # и добавить ихю
    
    # выбираем ссылки результатов запроса
    h <- xpathSApply(rootNode, 
                     '//h2[@class="serp-item__title"]/a | //div[@class="z-video__item"]/a |  //div[@class="z-video__item z-video__item_big-screen_yes"]/a',
                     xmlGetAttr, 'href')
   
    # ищем позицию(и), относящуюся к Яндекс Новостям
    u <- c(grep("http://news.yandex.ru/*", h))
    
  
    # собираем заголовки результатов запроса
    if (length(u) != 0) {
      urls_y <- c(urls_y, h[-u])
      # выбираем заголовки результатов запроса статей
      h <- xpathSApply(rootNode, '//h2[@class="serp-item__title"] | //span[@class = "link__inner z-video__link"]',
                       xmlValue)

      # собираем заголовки результатов запроса
      headers_y <- c(headers_y, h[-u])
      # заполняем столбец для лет
      years_y <- c(years_y, rep(n,length(h[-u])))
    } else {
      urls_y <- c(urls_y, h)
      # выбираем заголовки результатов запроса статей
      h <- xpathSApply(rootNode, '//h2[@class="serp-item__title"] | //span[@class = "link__inner z-video__link"]',
                       xmlValue)
      # собираем заголовки результатов запроса
      headers_y <- c(headers_y, h)
      # заполняем столбец для лет
      years_y <- c(years_y, rep(n,length(h)))
    }
      
    
    # выбираем все источники результатов запроса, исключая ссылки на ЯндексВидео
    # Мы это делаем по тем же причинам, что и когда исключали Яндекс Новости - 
    # это создает путаницу. Так, из-за Яндекс Видео мы найдем несколько лишних 
    # источников (sources_y), тогда как ни заголовков (headers_y), ни ссылок 
    # (urls_y) мы не найдем по используемым тегам.
    
    # выбираем ссылки результатов запроса
    h <- xpathSApply(rootNode, 
                     '//span[@class="serp-url__item"] | //span[@class="serp-url__item path"]',
                     xmlValue)
    # ищем позицию(и), относящуюся к Яндекс Видео
    u <- c(grep("video.yandex.ru", h))
    if (length(u) != 0) {
      sources_y <- c(sources_y, h[-u])
    } else {
      sources_y <- c(sources_y, h)
    }

    # Sys.sleep(40)
    
  }
  
}

# собираем в один фрейм
DF <- data.frame(Year = years_y, Header = headers_y,
                 Source = sources_y, URL = urls_y)

# Приведем заголовки к более красивому виду - уберем лишние ссылки и ...
DF$Header <- gsub(' [/||] .*$', '', DF$Header)
DF$Header <- gsub('[...]', '', DF$Header)

# запишем результат в файл
write.csv(DF, './Timeline.csv', row.names = F)
