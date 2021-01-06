# Library
library(jsonlite)
library(stringr)

fetchData <- function() {
  # Valiables
  arrayLength <- 1
  
  # Vectors
  valueString <- c("value")
  valueDecimal <- c(0)
  
  # Data Frames
  identifier <- data.frame(valueString)
  projectId <- data.frame(valueString)
  sequencingType <- data.frame(valueString)
  targetGene <- data.frame(valueString)
  specimenType <- data.frame(valueString)
  samplingDate <- data.frame(valueString)
  samplingYear <- data.frame(valueString)
  cellline <- data.frame(valueString)
  od260280 <- data.frame(valueDecimal)
  od260230 <- data.frame(valueDecimal)
  dnaIntegrity <- data.frame(valueDecimal)
  libraryInputAmount <- data.frame(valueDecimal)
  libraryInsertSize <- data.frame(valueDecimal)
  readLength <- data.frame(valueDecimal)
  totalReads <- data.frame(valueDecimal)
  meanCoverage <- data.frame(valueDecimal)
  uniformity <- data.frame(valueDecimal)
  onTargetRate <- data.frame(valueDecimal)
  q30 <- data.frame(valueDecimal)
  prScore <- data.frame(valueDecimal)
  
  # Fetch Data - REST API call
  getFhirResource <- fromJSON('https://skku-milab.ddns.net:9080/fhir/MolecularSequence')
  
  for (i in getFhirResource$entry$resource$identifier) {
    identifier <- rbind(identifier, data.frame(valueString=i$value))
    projectId <- rbind(projectId, data.frame(valueString=(str_replace(i$system, 'http://129.150.178.10:8080/qms/', ''))))
  }
  
  for (i in getFhirResource$entry$resource$extension) {
    arrayLength <- arrayLength + 1
    for (j in i$extension) {
      for (k in j$extension) {
        for (l in 1:length(k$url)) {
          if (k$url[l] == 'sequencingType') {
            sequencingType <- rbind(sequencingType, data.frame(valueString=k$valueString[l]))
          } else if (k$url[l] == 'targetGene') {
            targetGene <- rbind(targetGene, data.frame(valueString=k$valueString[l]))
          } else if (k$url[l] == 'specimenType') {
            specimenType <- rbind(specimenType, data.frame(valueString=k$valueString[l]))
          } else if (k$url[l] == 'samplingDate') {
            samplingDate <- rbind(samplingDate, data.frame(valueString=k$valueString[l]))
            samplingYear <- rbind(samplingYear, data.frame(valueString=str_sub(k$valueString[l], 1, 4)))
          } else if (k$url[l] == 'cellline') {
            cellline <- rbind(cellline, data.frame(valueString=k$valueString[l]))
          } else if (k$url[l] == 'dnaIntegrity') {
            dnaIntegrity <- rbind(dnaIntegrity, data.frame(valueDecimal=k$valueDecimal[l]))
          } else if (k$url[l] == 'libraryInputAmount') {
            libraryInputAmount <- rbind(libraryInputAmount, data.frame(valueDecimal=k$valueDecimal[l]))
          } else if (k$url[l] == 'libraryInsertSize') {
            libraryInsertSize <- rbind(libraryInsertSize, data.frame(valueDecimal=k$valueDecimal[l]))
          } else if (k$url[l] == 'readLength') {
            readLength <- rbind(readLength, data.frame(valueDecimal=k$valueDecimal[l]))
          } else if (k$url[l] == 'totalReads') {
            totalReads <- rbind(totalReads, data.frame(valueDecimal=k$valueDecimal[l]))
          } else if (k$url[l] == 'meanCoverage') {
            meanCoverage <- rbind(meanCoverage, data.frame(valueDecimal=k$valueDecimal[l]))
          } else if (k$url[l] == 'uniformity') {
            uniformity <- rbind(uniformity, data.frame(valueDecimal=k$valueDecimal[l]))
          } else if (k$url[l] == 'onTargetRate') {
            onTargetRate <- rbind(onTargetRate, data.frame(valueDecimal=k$valueDecimal[l]))
          } else if (k$url[l] == 'q30') {
            q30 <- rbind(q30, data.frame(valueDecimal=k$valueDecimal[l]))
          } else if (k$url[l] == 'prScore') {
            prScore <- rbind(prScore, data.frame(valueDecimal=k$valueDecimal[l]))
          } else if (k$url[l] == 'dnaPurity') {
            for (m in k$extension[l]){
              for (n in 1:length(m$url)) {
                if (m$url[n] == 'od260280') {
                  od260280 <- rbind(od260280, data.frame(valueDecimal=m$valueDecimal[n]))
                } else if (m$url[n] == 'od260230') {
                  od260230 <- rbind(od260230, data.frame(valueDecimal=m$valueDecimal[n]))
                }
              }
            }
          }
        }
      }
    }
    if (length( sequencingType$valueString         ) < arrayLength) { sequencingType         <- rbind(sequencingType         , data.frame( valueString = NA )) }
    if (length( targetGene$valueString             ) < arrayLength) { targetGene             <- rbind(targetGene             , data.frame( valueString = NA )) }
    if (length( specimenType$valueString           ) < arrayLength) { specimenType           <- rbind(specimenType           , data.frame( valueString = NA )) }
    if (length( samplingDate$valueString           ) < arrayLength) { samplingDate           <- rbind(samplingDate           , data.frame( valueString = NA )) }
    if (length( samplingYear$valueString           ) < arrayLength) { samplingYear           <- rbind(samplingYear           , data.frame( valueString = NA )) }
    if (length( cellline$valueString               ) < arrayLength) { cellline               <- rbind(cellline               , data.frame( valueString = NA )) }
    if (length( od260280$valueDecimal              ) < arrayLength) { od260280               <- rbind(od260280               , data.frame( valueDecimal= NA )) }
    if (length( od260230$valueDecimal              ) < arrayLength) { od260230               <- rbind(od260230               , data.frame( valueDecimal= NA )) }
    if (length( dnaIntegrity$valueDecimal          ) < arrayLength) { dnaIntegrity           <- rbind(dnaIntegrity           , data.frame( valueDecimal= NA )) }
    if (length( libraryInputAmount$valueDecimal    ) < arrayLength) { libraryInputAmount     <- rbind(libraryInputAmount     , data.frame( valueDecimal= NA )) }
    if (length( libraryInsertSize$valueDecimal     ) < arrayLength) { libraryInsertSize      <- rbind(libraryInsertSize      , data.frame( valueDecimal= NA )) }
    if (length( readLength$valueDecimal            ) < arrayLength) { readLength             <- rbind(readLength             , data.frame( valueDecimal= NA )) }
    if (length( totalReads$valueDecimal            ) < arrayLength) { totalReads             <- rbind(totalReads             , data.frame( valueDecimal= NA )) }
    if (length( meanCoverage$valueDecimal          ) < arrayLength) { meanCoverage           <- rbind(meanCoverage           , data.frame( valueDecimal= NA )) }
    if (length( uniformity$valueDecimal            ) < arrayLength) { uniformity             <- rbind(uniformity             , data.frame( valueDecimal= NA )) }
    if (length( onTargetRate$valueDecimal          ) < arrayLength) { onTargetRate           <- rbind(onTargetRate           , data.frame( valueDecimal= NA )) }
    if (length( q30$valueDecimal                   ) < arrayLength) { q30                    <- rbind(q30                    , data.frame( valueDecimal= NA )) }
    if (length( prScore$valueDecimal               ) < arrayLength) { prScore                <- rbind(prScore                , data.frame( valueDecimal= NA )) }
  }
  
  
  # Data Frames
  parsedData <- data.frame(
    identifier[-c(1),],
    projectId[-c(1),],
    sequencingType[-c(1),], 
    targetGene[-c(1),], 
    specimenType[-c(1),], 
    samplingDate[-c(1),], 
    cellline[-c(1),], 
    od260280[-c(1),], 
    od260230[-c(1),], 
    dnaIntegrity[-c(1),], 
    libraryInputAmount[-c(1),], 
    libraryInsertSize[-c(1),], 
    readLength[-c(1),], 
    totalReads[-c(1),], 
    meanCoverage[-c(1),], 
    uniformity[-c(1),], 
    onTargetRate[-c(1),], 
    q30[-c(1),], 
    prScore[-c(1),]
  )
  
  names(parsedData) <- c('identifier', 'projectId', 'sequencingType', 'targetGene', 'specimenType', 'samplingDate', 'cellline', 'od260280', 'od260230', 'dnaIntegrity', 'libraryInputAmount', 'libraryInsertSize', 'readLength', 'totalReads', 'meanCoverage', 'uniformity', 'onTargetRate', 'q30', 'prScore')
  
  return(parsedData)  
}
