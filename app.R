### Shiny app to viz nutrient predictions (JPWR)
# August 2022
# updated August 2023 with rfishbase values

## packages
library(shiny)
library(shinyjs)
library(tidyverse)
library(ggradar)

## plotting args
source('theme_sleek.R')
theme_set(theme_sleek())
pcols<-c(RColorBrewer::brewer.pal(9, 'Set1'), 'black') ## 10 colors

## get RDA reference vals
source('rda_reader.R')
rda$nutrient<-str_to_title(rda$nutrient)
rda$nutrient[rda$nutrient=='Vitamin_a']<-'Vitamina'
rda$nutrient[rda$nutrient=='Omega_3']<-'Omega3'

## get nutrient units
units<-data.frame(nutrient = c('Protein', 'Calcium', 'Iron', 'Selenium', 'Zinc', 'Omega3', 'VitaminA'),
                  unit = c('percent', 'mg', 'mg', 'mcg', 'mg', 'g', 'mcg'))

## load data
nut<-read.csv('Species_Nutrient_Predictions_muscle_wet_Jan2026.csv') %>% 
    # read.csv('Species_Nutrient_Predictions_muscle_wet_FB.csv') %>% 
            mutate(species =str_replace_all(species, '_', ' '), id = paste(species, 'tissue_wet'), form = 'Raw (fillet)')
                   
# nut_dry_whole<-read.csv('Species_Nutrient_Predictions_whole_dried.csv') %>% 
#             mutate(species =str_replace_all(species, '_', ' '), id = paste(species, 'whole_dry'), form = 'Dried (whole)')
# 
# nut_dry_tissue<-read.csv('Species_Nutrient_Predictions_muscle_dried.csv') %>% 
#             mutate(species =str_replace_all(species, '_', ' '), id = paste(species, 'tissue_dry'), form = 'Dried (fillet)')

## bind, remove any species-form duplicates
# nut<-rbind(nut, nut_dry_whole, nut_dry_tissue)
# nut<-nut[!duplicated(nut$id),]

## tidy names
nutl<-nut %>% 
    select(-SpecCode) %>% 
    pivot_longer(-c(species, id, form), names_to = 'temp', values_to = 'mu') %>% 
    mutate(temp = str_replace_all(temp, '_a', 'a'),
           temp = str_replace_all(temp, '_3', '3'),
           type = str_split_fixed(temp, '_', 2)[,2],
           nutrient = str_split_fixed(temp, '_', 2)[,1]) %>% 
    select(-temp) %>% 
    pivot_wider(names_from=type, values_from = mu) %>% 
    ## add RDA and units
    left_join(rda) %>% 
    left_join(units) %>% 
    mutate(rni_women = mu/rni_women*100,
           rni_kids = mu/rni_kids*100,
           rni_men = mu/rni_men*100,
           rni_pregnant = mu/rni_pregnant*100) %>% 
    mutate(nutrient = fct_relevel(nutrient, c('Protein', 'Calcium', 'Iron', 'Selenium', 'Zinc', 'Omega3', 'Vitamina'))) %>% 
    mutate(nutrient = recode(nutrient, Omega3 = 'Omega-3\nfatty acids', Vitamina = 'Vitamin A'))
    

## units in labels
nutl$lab<-nutl$nutrient
levels(nutl$lab)<-c("'Protein, g'", "'Calcium, mg'", "'Iron, mg'", expression('Selenium, '*mu*'g'), 
                    "'Zinc, mg'", "'Omega-3, g'", expression('Vitamin A, '*mu*'g'))

## get median nutrient values for reference in posterior plot
median_fish<-nutl %>% group_by(nutrient, lab, form) %>% summarise(med = median(mu))

## join common names
fb<-read.csv('Fishbase_species_names.csv')
nutl<-nutl %>% left_join(fb)

# Define UI for application that draws a histogram

ui <- fluidPage(
    useShinyjs(),
    theme = bslib::bs_theme(bootswatch = "lux"),
    headerPanel(div("Fish nutrient content", img(src = "FishNapp_logo.png", height=98, width=130)), windowTitle = 'Fish nutrient content'),
    p('Visualize and download nutrient concentrations for over 5,000 fish species'),
    sidebarLayout(
        # Sidebar panel for inputs
        sidebarPanel(
            selectizeInput("sp", label = "Scientific name", choices = NULL, multiple=TRUE),
            selectizeInput("name", label = "Common name", choices = NULL, multiple=TRUE),
            selectizeInput("form", label = "Food type", choices = NULL),
            selectizeInput("diet", label = "Dietary population", choices = NULL),
            sliderInput("portion", "Portion size, g", value = 100, min = 10, max = 250, step=10),
            h4('Background'),
            HTML(r"(
                 Nutrient values were predicted using a trait-based Bayesian model fitted to nutrient composition data from 610 fish species. Out-of-sample predictions were generated using trait values on Fishbase, which we
                extracted for over 5,000 fish species recorded in global fisheries datasets, including large- and small-scale fisheries and marine and freshwater species. Nutrient content data can be generated for 'wet' muscle tissue, based on the modelled differences between samples of raw muscle tissue (i.e. fillet), dried fish, and whole fish. Recommended intakes can be generated for specific portion sizes and dietary populations.
              <br> <br>
              Statistical model available <a href="https://github.com/mamacneil/NutrientFishbase/" target="_blank">here</a>. 
              Recommended nutrient intakes are available <a href=https://github.com/jpwrobinson/ShinyFishNutrients/blob/main/rda_reader.R target="_blank">here</a>. Minerals and vitamin A are recommended intakes from <a href="http://apps.who.int/iris/bitstream/handle/10665/42716/9241546123.pdf" target="_blank">WHO/FAO</a>,
              assuming 10% bioavailability for iron and moderate bioavailabity for zinc. 
              Omega-3 fatty acids are adequate intakes (AI) from <a href="https://pubmed.ncbi.nlm.nih.gov/12449285/" target="_blank">National Academies</a>.)"),
            h4('Read more'),
            HTML(r"(
                    <ul>
                    <li>MacNeil et al. in prep. Nutrient content estimation of global fishes.</li>
                    <li><a href="https://www.nature.com/articles/s41586-019-1592-6" target="_blank">Hicks et al. 2019. Harnessing global fisheries to tackle micronutrient deficiencies. Nature.</a></li>
                    <li><a href="https://www.cell.com/current-biology/fulltext/S0960-9822(21)00896-4" target="_blank">Maire et al. 2021. Micronutrient supply from global marine fisheries under climate change and overfishing. Current Biology.</a></li>
                    <li><a href="https://www.cell.com/one-earth/fulltext/S2590-3322(21)00723-5" target="_blank">Robinson et al. 2022. Climate-induced increases in micronutrient availability for coral reef fisheries. One Earth.</a></li>
                    <li><a href="https://www.pnas.org/doi/10.1073/pnas.2120817119" target="_blank">Nash et al. 2022. Trade and foreign fishing mediate global marine nutrient supply. PNAS.</a></li>
                    </ul>
                 )"),
            h4('Code'),
            HTML(r"(Created by James Robinson, with help from Kendra Byrd, Pip Cohen, Nick Graham, Christina Hicks, Aaron MacNeil, Eva Maire, and Sarah Martin. 
                 Data visualisation in R using tidyverse with ggradar, deployed using Shiny.)")),
        # Main panel for displaying outputs
        mainPanel(
            tabsetPanel(
                tabPanel("Recommend intakes", downloadButton('downloadP1', 'Save as PDF'), plotOutput('spider')),
                tabPanel("Nutrient concentrations", downloadButton('downloadP2', 'Save as PDF'), plotOutput('posteriors')),
                tabPanel("Download table",p(''), p(''),
                         downloadButton('download', 'Download .csv'),
                         tableOutput("table")
                         )
                )
        )
    )
)

server<-function(input, output, session) {
    

    observeEvent(length(c(input$sp, input$name)) == 0,
                 {shinyjs::enable("name")
                     shinyjs::enable("sp")})

    updateSelectizeInput(session, "form", choices = unique(nutl$form), server = TRUE)
    updateSelectizeInput(session, "diet", choices = c("Children (6mo - 5 yrs)", 'Adult women (18-65 yrs)', 'Pregnant women', 'Adult men (18-65 yrs)'), server = TRUE)
    updateSelectizeInput(session, 'name', choices = c(nutl$fbname), server = TRUE)
    updateSelectizeInput(session, 'sp', choices = c(nutl$species), server = TRUE)
    
    ## select species OR common name box
    observeEvent(c(input$name,input$sp),
                 {if(is.null(input$sp) & !is.null(input$name)) 
                     {disable("sp") & enable('name')} else {enable('sp') & disable('name')}})
    
    colSelect<-reactive({
        if(is.null(input$sp)) {c('fbname', 'nutrient', 'rni')} else
         {c('species', 'nutrient', 'rni')}})
    
    nutSelect<-reactive({
        validate(need(!is.null(c(input$sp, input$name)), 'To start, type one or more scientific OR common names'))
        if(is.null(input$sp)){
            req(input$name) 
            nutl$species[nutl$fbname %in% input$name]
        } else {req(input$sp) 
            input$sp}
    })
    nameSelect<-reactive({
        validate(need(!is.null(c(input$sp, input$name)), 'To start, type one or more scientific OR common names'))
        if(is.null(input$name)){
            req(input$sp) 
            nutl$fbname[nutl$species %in% input$sp]
        } else {req(input$name) 
            input$name}
    })

    frmSelect<-reactive({req(input$form) 
              input$form})
    rnSelect<-reactive({req(input$diet) 
              input$diet})
    ptnSelect<-reactive({req(input$portion) 
              input$portion})

    
    ## outputs
    
    spiderPlot <- reactive({
        
        ## arrange data
        dat<-nutl[nutl$species %in% nutSelect(),] %>% 
            filter(fbname %in% nameSelect()) %>% 
            filter(form == frmSelect()) %>% 
            filter(nutrient != 'Protein') %>% 
            ungroup() %>% 
            mutate(rni = case_when(str_detect(rnSelect(), 'Children') ~ rni_kids, 
                                   str_detect(rnSelect(), 'Adult women')~rni_women,
                                   str_detect(rnSelect(), 'Adult men')~rni_men,
                                   str_detect(rnSelect(), 'Pregnant')~rni_pregnant)) %>% 
            mutate(rni = rni / (100/ptnSelect())) %>% 
            mutate(rni = rni/100) %>%
            ## cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
            mutate(rni = case_when(rni > 1 ~ 1, TRUE ~ rni))
        
        if(colSelect()[1] == 'fbname'){
            fbname_long<-paste0(unique(dat$fbname), ' = ', unique(dat$species), collapse='\n')} 
        else
            {fbname_long<-paste0(unique(dat$species), ' = ', unique(dat$fbname), collapse='\n')}
    
        dat<-dat[,colSelect()]
        dat<-dat %>% pivot_wider(names_from = nutrient, values_from = rni)
        
        
        tit<-if(str_detect(rnSelect(), 'Children')){
                    paste0('children (6 mo - 5 yrs) from a ', ptnSelect(), ' g portion')} else 
                      if(str_detect(rnSelect(), 'Adult women')){
                    paste0('adult women (18-65) from a ', ptnSelect(), ' g portion')} else
                      if(str_detect(rnSelect(), 'Adult men')){
                        paste0('adult men (18-65) from a ', ptnSelect(), ' g portion')} else
                          if(str_detect(rnSelect(), 'Pregnant')){
                            paste0('pregnant women from a ', ptnSelect(), ' g portion')}
        tit<-paste0('\n\nRecommended intakes for ', tit)
        subtit<-'\nRadar plots show the contribution of a single fish portion to recommended daily nutrient intakes (capped at 100%).'
        cap<-paste('\n\n\n', fbname_long)
        
        ggradar(dat, 
                        group.colours = pcols,
                        base.size = 1,
                        group.point.size = 2,
                        group.line.width = 1,
                        background.circle.colour = "white",
                        axis.label.size = 4,
                        fill=TRUE,
                        gridline.mid.colour = "grey") + 
                labs(
                    title = tit,
                    subtitle = subtit,
                    caption = cap
                    ) +
                coord_equal(clip='off') +
            theme(
                plot.title = element_text(size=14, colour='black', face=2, hjust=1),
                plot.subtitle = element_text(size=11, colour='black', face=3, hjust=1),
                plot.caption = element_text(size=12, colour='#636363', face=3),
                legend.text = element_text(size = 11))
        
    })
    
    postPlot <- reactive({
        
        dat2<-nutl[nutl$species %in% nutSelect(),] %>%
            filter(form == frmSelect()) %>% 
            filter(fbname %in% nameSelect()) 
        
        dat2$species<-factor(dat2$species, levels = unique(nutSelect()))
        
        if(colSelect()[1] == 'fbname'){
        fbname_long<-paste0(unique(dat2$fbname), ' = ', unique(dat2$species), collapse='\n')} else
            {fbname_long<-paste0(unique(dat2$species), ' = ', unique(dat2$fbname), collapse='\n')}
            
        median_dat<-median_fish %>% filter(form == frmSelect())
        
        cap2<-paste0('\n\nDashed grey line is median value across all fish species.\nPoints are median value for each species, with 95% certainty intervals.\nNutrient units in panel titles.\n\n',
                     fbname_long)
        
        ggplot(dat2, aes(col=species)) + 
            geom_hline(data = median_dat, aes(yintercept = med), linetype=5, col='grey50') +
            geom_pointrange(aes(species, mu, ymin = l95, ymax = u95)) +
            # geom_pointrange(aes(species, mu, ymin = l50, ymax = u50), size=1.2, fatten=0) +
            geom_point(aes(species, mu)) +
            facet_wrap(~lab, scales='free', nrow=1, labeller=label_parsed) +
            labs(x = '', y = 'concentration per 100 g', 
                 # title = 'Posterior predicted nutrient concentration',
                 caption = cap2) +
            scale_colour_manual(values = pcols) +
            theme(axis.ticks.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.line.x = element_blank(),
                  strip.text.x = element_text(size = 12),
                  legend.title=element_blank(),
                  plot.caption = element_text(size=12, colour='#636363', face=3),
                    plot.title = element_text(size=14, colour='black', face=2),
                  legend.text = element_text(size = 11))
    })
    
    output$spider<- renderPlot({spiderPlot()})
    
    output$downloadP1 <- downloadHandler(
        filename = 'Species_RNI.pdf', 
        content = function(file){
        ggsave(file, spiderPlot(), height = 5, width = 12)
    })
    
    output$posteriors<- renderPlot({postPlot()})
    output$downloadP2 <- downloadHandler(
        filename = 'Species_posteriors.pdf', 
        content = function(file){
            ggsave(file, postPlot(), height = 7, width = 12)
        })
    
    tabber<-reactive({  nutl[nutl$species %in% nutSelect(),] %>%
            filter(fbname %in% nameSelect()) %>% 
            filter(form == frmSelect()) %>% 
            select(species, fbname, form, nutrient, mu:u95, unit, rni_women, rni_men, rni_pregnant, rni_kids) %>%  
            rename('Concentration_per_100g'  = mu,
                   'Lower 95%'  = l95,
                   'Upper 95%'  = u95,
                   # 'Lower 50%'  = l50,
                   # 'Upper 50%'  = u50,
                   'Nutrient' = nutrient,
                   'Unit' = unit,
                   'Species' = species,
                   'FishbaseName' = fbname,
                   'RNI_adult_women_18-65' = rni_women,
                   'RNI_adult_men_18-65' = rni_men,
                   'RNI_pregnant_women' = rni_pregnant,
                   'RNI_children_6mo-5yr' = rni_kids) %>% 
            arrange(Nutrient) })
    
    output$download <- downloadHandler(
        filename = "Species_Nutrient_Predictions.csv",
        content = function(file) {
            write.csv(tabber(), file)
        })
    
    output$table <- renderTable({tabber()})

}

# Run the application 
shinyApp(ui = ui, server = server)

