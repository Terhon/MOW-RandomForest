 nodeAttributesChoice.init <- function(attributesToChooseCount, allAttributesCount) {
  nodeAttributesChoiceInfo <- createEnvironment()
  nodeAttributesChoiceInfo$attributesToChooseCount <- attributesToChooseCount
  nodeAttributesChoiceInfo$allAttributesCount <- allAttributesCount
  nodeAttributesChoiceInfo
 }

 nodeAttributesChoice.chooseAttributes <- function(info) {
   chosenAttributesIndexes <- sample.int(info$allAttributesCount, info$attributesToChooseCount)
   info$currentAttribute <- 1
   info$choosenAttributes <- chosenAttributesIndexes
   info
 }

 nodeAttributesChoice.isAttributeChosen <- function(info) {
   attributeMask <- info$choosenAttributes == info$currentAttribute
   foundAttributeCount = sum(attributeMask)

   if (foundAttributeCount > 0) {
      info$choosenAttributes <- info$choosenAttributes[!attributeMask]
   }

   info$currentAttribute <- info$currentAttribute + 1

   foundAttributeCount > 0
 }
