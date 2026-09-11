FROM inwt/r-shiny:4.4.3

COPY . .

RUN installPackage
