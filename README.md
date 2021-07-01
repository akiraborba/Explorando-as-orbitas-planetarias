#### Um pouco sobre astronomia
As culturas humanas sempre atribuíram significado ao céu e aos ciclos astronômicos (astrológicos). No entanto, por termos um ponto de vista limitado desses fenômenos, sempre observados da posição da Terra (a menos que você esteja lá na Estação Espacial), a compreensão da população sobre astronomia é restrita e sujeita a concepções errôneas que ainda sobrevivem no dia de hoje. Portanto, iniciativas de divulgação do conhecimento astronômico são essenciais, e se constituem como movimentos em direção a uma cidadania plena.

#### Criando as órbitas dos planetas
Este web app foi pensado como uma ferramenta de visualização das escalas das órbitas dos planetas (e planetas-anões porque eu não posso deixar Plutão de fora). Para isso, é preciso criar uma escala numérica que represente estas órbitas. Serão criados vetores que vão de 0 a 1 e de volta ao 0 para representar isso. Então, se a órbita de um planeta fosse de 5 dias terrestres, seu vetor seria 0, 0.5, 1, 0.5, 0. Observe que isto não corresponde precisamente a órbitas eliptícas, que são irregulares, mas permitirão uma escala comparativa. As órbitas foram arrendodas para números pares, em dias terrestres, para facilitar a programação.

```{r}
    Mercury <- c(0 + (1:44)/44, 1 - (1:44)/44)
    Venus <- c(0 + (1:112)/112, 1 - (1:112)/112)
    Earth <- c(0 + (1:182)/182, 1 - (1:182)/182)
    Mars <- c(0 + (1:343)/343, 1 - (1:343)/343)
    Jupiter <- c(0 + (1:2165)/2165, 1 - (1:2165)/2165)
    Saturn <- c(0 + (1:5376)/5376, 1 - (1:5376)/5376)
    Uranus <- c(0 + (1:15317)/15317, 1 - (1:15317)/15317)
    Neptune <- c(0 + (1:30074)/30074, 1 - (1:30074)/30074)
    Pluto <- c(0 + (1:45368)/45368, 1 - (1:45368)/45368)
    Time <- c(1:90736)
````

#### Possibilitando transformações
Para adicionar um componente mais interativo, o usuário vai poder realizar duas transformações. A primeira, a de escolher quantos dias terrestres serão representados (de 1 a 90736, um ano em plutão) e como os números devem ser representados (retos, formando triângulos, ou em função exponencial, logarítima ou uma função combinada, apresentada ao usuário como "loucura). 

```{r}
    planet.data <-
        reactive({
            if(desired.transformation() == "str" ) {
                Mercury.str <- rep(Mercury, length.out = input$slider)
                Venus.str <- rep(Venus, length.out = input$slider)
                Earth.str <- rep(Earth, length.out = input$slider)
                Mars.str <- rep(Mars, length.out = input$slider)
                Jupiter.str <- rep(Jupiter, length.out = input$slider)
                Saturn.str <- rep(Saturn, length.out = input$slider)
                Uranus.str <- rep(Uranus, length.out = input$slider)
                Neptune.str <- rep(Neptune, length.out = input$slider)
                Pluto.str <- Pluto[1:input$slider]
                Time.str <- Time[1:input$slider]
                data.frame("Mercury.plot" = Mercury.str, 
                           "Venus.plot" = Venus.str,
                           "Earth.plot" = Earth.str, 
                           "Mars.plot" = Mars.str, 
                           "Jupiter.plot" = Jupiter.str, 
                           "Saturn.plot" = Saturn.str, 
                           "Uranus.plot" = Uranus.str, 
                           "Neptune.plot" = Neptune.str, 
                           "Pluto.plot" = Pluto.str,
                           "Time.plot" = Time.str)}
            else{
                if(desired.transformation() == "log") {
                    Mercury.log <- rep(log(Mercury), length.out = input$slider)
                    Venus.log <- rep(log(Venus), length.out = input$slider)
                    Earth.log <- rep(log(Earth), length.out = input$slider)
                    Mars.log <- rep(log(Mars), length.out = input$slider)
                    Jupiter.log <- rep(log(Jupiter), length.out = input$slider)
                    Saturn.log <- rep(log(Saturn), length.out = input$slider)
                    Uranus.log <- rep(log(Uranus), length.out = input$slider)
                    Neptune.log <- rep(log(Neptune), length.out = input$slider)
                    Pluto.log <- log(Pluto[1:input$slider])
                    Time.log <- Time[1:input$slider]
                    data.frame(
                        "Mercury.plot" = Mercury.log,
                        "Venus.plot" = Venus.log,
                        "Earth.plot" = Earth.log,
                        "Mars.plot" = Mars.log,
                        "Jupiter.plot" = Jupiter.log,
                        "Saturn.plot" = Saturn.log,
                        "Uranus.plot" = Uranus.log,
                        "Neptune.plot" = Neptune.log,
                        "Pluto.plot" = Pluto.log,
                        "Time.plot" = Time.log)}
                else{
                    if(desired.transformation() == "exp") {
                        Mercury.exp <- rep(exp(Mercury), length.out = input$slider)
                        Venus.exp <- rep(exp(Venus), length.out = input$slider)
                        Earth.exp <- rep(exp(Earth), length.out = input$slider)
                        Mars.exp <- rep(exp(Mars), length.out = input$slider)
                        Jupiter.exp <- rep(exp(Jupiter), length.out = input$slider)
                        Saturn.exp <- rep(exp(Saturn), length.out = input$slider)
                        Uranus.exp <- rep(exp(Uranus), length.out = input$slider)
                        Neptune.exp <- rep(exp(Neptune), length.out = input$slider)
                        Pluto.exp <- exp(Pluto)[1:input$slider]
                        Time.exp <- Time[1:input$slider]
                        data.frame("Mercury.plot" = Mercury.exp, 
                                   "Venus.plot" = Venus.exp,
                                   "Earth.plot" = Earth.exp, 
                                   "Mars.plot" = Mars.exp, 
                                   "Jupiter.plot" = Jupiter.exp, 
                                   "Saturn.plot" = Saturn.exp, 
                                   "Uranus.plot" = Uranus.exp,
                                   "Neptune.plot" = Neptune.exp, 
                                   "Pluto.plot" = Pluto.exp,
                                   "Time.plot" = Time.exp)}
                    else{
                        if(desired.transformation() == "crazy") {
                            Mercury.exp <- rep((log(Mercury)*Mercury^2), length.out = input$slider)
                            Venus.exp <- rep((log(Venus)*Venus^2), length.out = input$slider)
                            Earth.exp <- rep((log(Earth)*Earth^2), length.out = input$slider)
                            Mars.exp <- rep((log(Mars)*Mars^2), length.out = input$slider)
                            Jupiter.exp <- rep((log(Jupiter)*Jupiter^2), length.out = input$slider)
                            Saturn.exp <- rep((log(Saturn)*Saturn^2), length.out = input$slider)
                            Uranus.exp <- rep((log(Uranus)*Uranus^2), length.out = input$slider)
                            Neptune.exp <- rep((log(Neptune)*Neptune^2), length.out = input$slider)
                            Pluto.exp <- (log(Pluto)*Pluto^2)[1:input$slider]
                            Time.exp <- Time[1:input$slider]
                            data.frame("Mercury.plot" = Mercury.exp, 
                                       "Venus.plot" = Venus.exp,
                                       "Earth.plot" = Earth.exp, 
                                       "Mars.plot" = Mars.exp, 
                                       "Jupiter.plot" = Jupiter.exp, 
                                       "Saturn.plot" = Saturn.exp, 
                                       "Uranus.plot" = Uranus.exp,
                                       "Neptune.plot" = Neptune.exp, 
                                       "Pluto.plot" = Pluto.exp,
                                       "Time.plot" = Time.exp)}}}}})
```
#### Criando a representação gráfica
Finalmente, com os dados selecionados e transformados de acordo com o desejo do usuário, uma representação gráfica pode ser programada!
```{r}
    output$planets.plot <- renderPlot({
        planet.plot <- ggplot(data = planet.data(), aes(x = Time.plot)) +
            theme(legend.title = element_blank(), legend.position = "bottom") +
            labs(x = "Earth Days", y = "Cycle Representation", title = "Planet's orbit cyle graphical representation")
        if("Mercury" %in% input$planets){
            planet.plot <- planet.plot + geom_line(data = planet.data(), aes(y = Mercury.plot, color = "Mercury"))}
        if("Venus" %in% input$planets){
            planet.plot <- planet.plot + geom_line(data  = planet.data(), aes(y = Venus.plot, color = "Venus"))}
        if("Earth" %in% input$planets){
            planet.plot <- planet.plot + geom_line(data = planet.data(), aes(y = Earth.plot, color = "Earth"))}
        if("Mars" %in% input$planets){
            planet.plot <- planet.plot + geom_line(data = planet.data(), aes(y = Mars.plot, color = "Mars"))}
        if("Jupiter" %in% input$planets){
            planet.plot <- planet.plot + geom_line(data  = planet.data(), aes(y = Jupiter.plot, color = "Jupiter"))}
        if("Saturn" %in% input$planets){
            planet.plot <- planet.plot + geom_line(data = planet.data(), aes(y = Saturn.plot, color = "Saturn"))}
        if("Uranus" %in% input$planets){
            planet.plot <- planet.plot + geom_line(data = planet.data(), aes(y = Uranus.plot, color = "Uranus"))}
        if("Neptune" %in% input$planets){
            planet.plot <- planet.plot + geom_line(data = planet.data(), aes(y = Neptune.plot, color = "Neptune"))}
        if("Pluto" %in% input$planets){
            planet.plot <- planet.plot + geom_line(data = planet.data(), aes(y = Pluto.plot, color = "Pluto"))}
        planet.plot})
```
#### Divirta-se!
Pronto, tudo está nos eixos. Agora é só disponibilizar aos usuários. Você pode usar e se divertir com ela  no https://arthurluna.shinyapps.io/SolarSysPlot/ ! Observe que ela foi feita em inglês. Se você deseja usar essa ferramenta e precisa dela em português, entra em contato comigo!

