

# Load một số package cần thiết: 

library(rvest)
library(tidyverse)
library(magrittr)
library(stringr)


#======================
#       Bán Hàng
#======================

base_url <- "https://viectotnhat.com/viec-lam-ban-hang-f10.html?"
n_page_bh <- 55
all_pages_ban_hang <- paste0(base_url, paste0("page=", 1:n_page_bh))


# Viết hàm lấy các thông tin về việc làm được tuyển dụng của page: 

job_crawler <- function(your_link) {
  
  h1 <- read_html(your_link)
  h2 <- html_nodes(h1, xpath = '//*[@id="vieclam-nganh"]/div[2]') %>% 
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  str_locate_all(h2, "/") %>% 
    as.data.frame() %>% 
    pull(start) -> vi_tri 
  
  
  vi_tri %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    as.data.frame() -> vi_tri
  
  vi_tri <- vi_tri$V2
  
  #  Thông tin về công việc đầu tiên trong page: 
  str_sub(h2, start = 1, end = vi_tri[1] + 4) %>% 
    str_split("   ") %>% 
    unlist() -> p
  
  p[str_count(p) != 0] -> r1
  
  # Các công việc từ vị trí 2 đến 15: 
  
  cong_viec <- c()
  
  for (j in 1:14) {
    
    str_sub(h2, start = vi_tri[j] + 5, end = vi_tri[j + 1] + 4) %>% 
      str_split("   ") %>% 
      unlist() -> p
    
    p[str_count(p) != 0] -> r
    cong_viec <- c(cong_viec, r)
    
  }
  
  tat_ca_cong_viec <- c(r1, cong_viec)
  
  tat_ca_cong_viec %<>% matrix(ncol = 5, byrow = TRUE) %>% as.data.frame()
  
  names(tat_ca_cong_viec) <- c("job_name", "company", "salary", "location", "deadline")
  
  
  tat_ca_cong_viec %<>% mutate(link_source = your_link)
  
  str_locate_all(your_link, "com/") %>% as.data.frame() -> loc1
  str_locate_all(your_link, "html") %>% as.data.frame() -> loc2
  
  job_type <- str_sub(your_link, start = loc1$start + 4, end = loc2$end - 9) %>% 
    str_replace_all("-", "_")
  
  tat_ca_cong_viec %<>% 
    mutate(job_type = job_type)
  
  tat_ca_cong_viec %<>% 
    mutate_if(is.factor, as.character) %>% 
    filter(str_detect(salary, pattern = "triệu"))
  
  return(tat_ca_cong_viec)
  
}


# Sử dụng hàm để lấy dữ liệu cho nhóm việc bán hàng: 


job_list_bh <- vector("list", length = n_page_bh)

for (i in seq_along(job_list_bh)) {
  job_list_bh[[i]] <- job_crawler(all_pages_ban_hang[i])
  Sys.sleep(1)
}


all_job_bh <- do.call("bind_rows", job_list_bh)



#======================
#       Kinh Doanh
#======================

base_url <- "https://viectotnhat.com/viec-lam-kinh-doanh-f32.html?"
n_page_kd <- 61
all_pages_kinh_doanh <- paste0(base_url, paste0("page=", 1:n_page_kd))


job_list_kd <- vector("list", length = n_page_kd)

for (i in seq_along(job_list_kd)) {
  job_list_kd[[i]] <- job_crawler(all_pages_kinh_doanh[i])
  Sys.sleep(1)
}


all_job_kd <- do.call("bind_rows", job_list_kd)



#=============================
#    Chăm sóc khách hàng
#=============================

base_url <- "https://viectotnhat.com/viec-lam-cham-soc-khach-hang-f21.html?"
n_page_cskh <- 35
all_pages_cskh <- paste0(base_url, paste0("page=", 1:n_page_cskh))


job_list_cskh <- vector("list", length = n_page_cskh)

for (i in seq_along(job_list_cskh)) {
  job_list_cskh[[i]] <- job_crawler(all_pages_cskh[i])
  Sys.sleep(1)
}


all_job_cskh <- do.call("bind_rows", job_list_cskh)



#=================================
#  Hành chính - trợ lí - thư kí
#=================================

base_url <- "https://viectotnhat.com/viec-lam-hanh-chinh-thu-ky-tro-ly-f29.html?"
n_page_ass <- 24
all_pages_ass <- paste0(base_url, paste0("page=", 1:n_page_ass))


job_list_ass <- vector("list", length = n_page_ass)

for (i in seq_along(job_list_ass)) {
  job_list_ass[[i]] <- job_crawler(all_pages_ass[i])
  Sys.sleep(1)
}


all_job_ass <- do.call("bind_rows", job_list_ass)



#==============================
#          Intership
#==============================

base_url <- "https://viectotnhat.com/viec-lam-sinh-vien-moi-tot-nghiep-thuc-tap-f35.html?"
n_page_int <- 18
all_pages_int <- paste0(base_url, paste0("page=", 1:n_page_int))


job_list_int <- vector("list", length = n_page_int)

for (i in seq_along(job_list_int)) {
  job_list_int[[i]] <- job_crawler(all_pages_int[i])
  Sys.sleep(1)
}


all_job_int <- do.call("bind_rows", job_list_int)



#===================================
#      Lao động phổ thông
#===================================


base_url <- "https://viectotnhat.com/viec-lam-lao-dong-pho-thong-f33.html?"
n_page_uns <- 22
all_pages_uns <- paste0(base_url, paste0("page=", 1:n_page_uns))


job_list_uns <- vector("list", length = n_page_uns)

for (i in seq_along(job_list_uns)) {
  job_list_uns[[i]] <- job_crawler(all_pages_uns[i])
  Sys.sleep(1)
}


all_job_uns <- do.call("bind_rows", job_list_uns)

#===================================
#       Tài Chính - Kế Toán
#===================================

base_url <- "https://viectotnhat.com/viec-lam-tai-chinh-ke-toan-kiem-toan-f47.html?"
n_page_fin <- 23
all_pages_fin <- paste0(base_url, paste0("page=", 1:n_page_fin))


job_list_fin <- vector("list", length = n_page_fin)

for (i in seq_along(job_list_fin)) {
  job_list_fin[[i]] <- job_crawler(all_pages_fin[i])
  Sys.sleep(1)
}


all_job_fin <- do.call("bind_rows", job_list_fin)


#=====================================
#             Marketing
#=====================================

base_url <- "https://viectotnhat.com/viec-lam-quang-cao-marketing-pr-f45.html?"
n_page_mar <- 24
all_pages_mar <- paste0(base_url, paste0("page=", 1:n_page_mar))


job_list_mar <- vector("list", length = n_page_mar)

for (i in seq_along(job_list_mar)) {
  job_list_mar[[i]] <- job_crawler(all_pages_mar[i])
  Sys.sleep(1)
}


all_job_mar <- do.call("bind_rows", job_list_mar)


#========================
#      Bất động sản
#========================

base_url <- "https://viectotnhat.com/viec-lam-bat-dong-san-f13.html?"
n_page_bds <- 11
all_pages_bds <- paste0(base_url, paste0("page=", 1:n_page_bds))


job_list_bds <- vector("list", length = n_page_bds)

for (i in seq_along(job_list_bds)) {
  job_list_bds[[i]] <- job_crawler(all_pages_bds[i])
  Sys.sleep(1)
}


all_job_bds <- do.call("bind_rows", job_list_bds)


#===============================
#    Công nghệ thông tin 
#===============================

base_url <- "https://viectotnhat.com/viec-lam-cong-nghe-thong-tin-f17.html?"
n_page_it <- 18
all_pages_it <- paste0(base_url, paste0("page=", 1:n_page_it))


job_list_it <- vector("list", length = n_page_it)

for (i in seq_along(job_list_it)) {
  job_list_it[[i]] <- job_crawler(all_pages_it[i])
  Sys.sleep(1)
}


all_job_it <- do.call("bind_rows", job_list_it)


#===================================
#    Nhà hàng - khách sạn
#===================================

base_url <- "https://viectotnhat.com/viec-lam-du-lich-nha-hang-khach-san-f23.html?"
n_page_it <- 11
all_pages_it <- paste0(base_url, paste0("page=", 1:n_page_it))


job_list_it <- vector("list", length = n_page_it)

for (i in seq_along(job_list_it)) {
  job_list_it[[i]] <- job_crawler(all_pages_it[i])
  Sys.sleep(1)
}


all_job_nhks <- do.call("bind_rows", job_list_it)

#================================
#           Cơ Khí
#================================


base_url <- "https://viectotnhat.com/viec-lam-co-khi-ki-thuat-ung-dung-f16.html?"
n_page_it <- 16
all_pages_it <- paste0(base_url, paste0("page=", 1:n_page_it))


job_list_it <- vector("list", length = n_page_it)

for (i in seq_along(job_list_it)) {
  job_list_it[[i]] <- job_crawler(all_pages_it[i])
  Sys.sleep(1)
}


all_job_ck <- do.call("bind_rows", job_list_it)


#=================================
#      Ngành Điện
#=================================

base_url <- "https://viectotnhat.com/viec-lam-dien-dien-tu-dien-lanh-f22.html?"
n_page_it <- 12
all_pages_it <- paste0(base_url, paste0("page=", 1:n_page_it))


job_list_it <- vector("list", length = n_page_it)

for (i in seq_along(job_list_it)) {
  job_list_it[[i]] <- job_crawler(all_pages_it[i])
  Sys.sleep(1)
}


all_job_dien <- do.call("bind_rows", job_list_it)

#=============================
#        Nhân sự
#============================


base_url <- "https://viectotnhat.com/viec-lam-nhan-su-f40.html?"
n_page_it <- 9
all_pages_it <- paste0(base_url, paste0("page=", 1:n_page_it))


job_list_it <- vector("list", length = n_page_it)

for (i in seq_along(job_list_it)) {
  job_list_it[[i]] <- job_crawler(all_pages_it[i])
  Sys.sleep(1)
}


all_job_nhan_su <- do.call("bind_rows", job_list_it)


#======================
#    Thiết kế
#======================

base_url <- "https://viectotnhat.com/viec-lam-thiet-ke-my-thuat-f49.html?"
n_page_it <- 10
all_pages_it <- paste0(base_url, paste0("page=", 1:n_page_it))


job_list_it <- vector("list", length = n_page_it)

for (i in seq_along(job_list_it)) {
  job_list_it[[i]] <- job_crawler(all_pages_it[i])
  Sys.sleep(1)
}


all_job_thiet_ke <- do.call("bind_rows", job_list_it)


#===================
#   Xây dựng
#===================


base_url <- "https://viectotnhat.com/viec-lam-xay-dung-f52.html?"
n_page_it <- 11
all_pages_it <- paste0(base_url, paste0("page=", 1:n_page_it))


job_list_it <- vector("list", length = n_page_it)

for (i in seq_along(job_list_it)) {
  job_list_it[[i]] <- job_crawler(all_pages_it[i])
  Sys.sleep(1)
}


all_job_xay_dung <- do.call("bind_rows", job_list_it)

#=====================
#  Tổng hợp dữ liệu
#=====================

all_df <- bind_rows(all_job_ass, 
                    all_job_bds, 
                    all_job_bh, 
                    all_job_ck, 
                    all_job_cskh, 
                    all_job_dien, 
                    all_job_fin, 
                    all_job_int, 
                    all_job_it, 
                    all_job_kd, 
                    all_job_mar, 
                    all_job_nhan_su, 
                    all_job_nhks, 
                    all_job_thiet_ke, 
                    all_job_uns, 
                    all_job_xay_dung)

# Lưu lại dữ liệu ở ổ D của máy tính: 
write.csv(all_df, "D:/all_jobs.csv", row.names = FALSE)



