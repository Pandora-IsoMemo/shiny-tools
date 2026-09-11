FROM inwt/r-shiny:4.4.3

RUN echo "options(repos = c(getOption('repos'), PANDORA = 'https://Pandora-IsoMemo.github.io/drat/'))" >> /usr/local/lib/R/etc/Rprofile.site

COPY . .

RUN installPackage
