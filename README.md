### Identificação:

Nome: Diogo Dalbianco dos Santos
Curso: Sistemas de Informação

### Tema/objetivo: 

Proposta baseada nas fullbottles presentes na série Kamen Rider Build, as quais os personagens utilizam para realizar suas transformações.

A ideia consiste em criar um banco de dados contendo todos os frascos da série. Ao selecionar uma Fullbottle, o sistema retornará:
* Informações sobre a Fullbottle escolhida

* Imagem correspondente

* Indicação de qual seria a outra Fullbottle necessária para formar a melhor combinação

Ao selecionar duas Fullbottles, o sistema verificará:

* Se a combinação é a ideal (ou não)

* Se essa combinação apareceu na série

* Caso tenha aparecido, apresentará informações sobre a combinação

### Processo de desenvolvimento:

Nas primeiras etapas, foi necessário estudar o SQLite para compreender o funcionamento do banco de dados. As versões iniciais apresentaram problemas devido à ausência de detalhes importantes na estrutura do banco.

Em relação ao código em Haskell, a dificuldade inicial foi compreender o programa de exemplo fornecido pela professora. Ao longo do desenvolvimento, pude aprender e aplicar os conceitos necessários. Optei por implementar um frontend para organizar melhor a exibição das informações, já que a interface precisava apresentar diversos elementos de forma clara. Por não ter muita experiência com HTML, utilizei algoritmos gerados artificialmente como base para os scripts, uma vez que essa parte não foi abordada em profundidade durante o curso.

No meio do desenvolvimento, enfrentei problemas para exibir imagens a partir do banco de dados no ambiente Haskell. O programa retornava erros frequentes devido a incompatibilidades com as bibliotecas importadas, o que dificultou a implementação completa dessa funcionalidade. Ainda assim, tenho interesse em explorar soluções para essa questão no futuro.

Ao final do trabalho, consegui implementar as funções restantes, compreendendo melhor como manipular o banco de dados e utilizar JSON.

Tenho o objetivo de dar continuidade ao projeto, aprofundando meus conhecimentos em HTML e CSS para criar um frontend totalmente autoral e mais elaborado. Além disso, pretendo explorar bibliotecas que permitam resolver os problemas relacionados à exibição de imagens. Outras ideias para expansão incluem a implementação de outras mecânicas da série, como a reprodução de sons associados às Fullbottles e combinações.

### Orientações para execução:

Eu apenas usei o codespaces do repositorio e ultilizei o comando "runhaskell main.hs" para testar

### Resultado final:
<video src="C:\Users\diogo\Videos\2025-09-28 18-27-19.mkv" width="320" height="240" controls></video>

### Referências e créditos

## Dados para o banco
<[Fullbottles](https://kamenrider.fandom.com/wiki/Fullbottles#Abiotic)>
<[Formas/Combinações](https://kamenrider.fandom.com/wiki/Kamen_Rider_Build_(Rider)?file=KRBu-Buildgorillamond.png#Tank)>

## Referencia
<https://www-w3schools-com.translate.goog/tags/tag_select.asp?_x_tr_sl=en&_x_tr_tl=pt&_x_tr_hl=pt&_x_tr_pto=tc>
<https://developer-mozilla-org.translate.goog/en-US/docs/Web/HTML/Reference/Elements/select?_x_tr_sl=en&_x_tr_tl=pt&_x_tr_hl=pt&_x_tr_pto=tc>
<https://github.com/AndreaInfUFSM/elc117-2025b?tab=readme-ov-file>
<https://youtu.be/IWPfBuSlLjA?si=cHTjYnZbF1PV5ITZ>
<https://www.sqlitetutorial.net>
<https://youtu.be/psTTKGj9G6Y?si=wgvYm5s1TOSKryGI>
<https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/script>
<https://www.w3schools.com/tags/tag_script.asp>



