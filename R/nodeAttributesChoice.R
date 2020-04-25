#' Creates environment structure containing basic information about choosing attributes in trees.
#'
#' @param attributesToChooseCount Number of attributes to choose in tree.
#' @param allAttributesCount Total count of attributes in tree.
#'
#' @return NodeAttributesChoiceInfo environment structure containing basic information about choosing tree attributes.
#'
#' @examples
#' attributesToChooseCount <- 2
#' allAttributesCount <- 3
#' nodeAttributesChoice.init(attributesToChooseCount, allAttributesCount)
#'
nodeAttributesChoice.init <- function(attributesToChooseCount, allAttributesCount) {
   if (attributesToChooseCount > allAttributesCount){
      stop("AttributesToChooseCount has to be smaller or equal than AllAttributesCount")
   }

   nodeAttributesChoiceInfo <- createEnvironment()
   nodeAttributesChoiceInfo$attributesToChooseCount <- attributesToChooseCount
   nodeAttributesChoiceInfo$allAttributesCount <- allAttributesCount
   nodeAttributesChoiceInfo
}

#' Chooses specific attributes in tree.
#'
#' Chooses specific attributes in tree and stores its indexes to NodeAttributesChoiceInfo structure.
#' Adds initial attribute index from which start choosing attributes.
#'
#' @param info NodeAttributesChoice structure
#'
#' @return NodeAttributesChoice structure
#'
#' @examples
#' nodeAttributesChoiceInfo <- nodeAttributesChoice.init(2, 3)
#' pnodeAttributesChoice.chooseAttributes(nodeAttributesChoiceInfo)
#'
nodeAttributesChoice.chooseAttributes <- function(info) {
   chosenAttributesIndexes <- sample.int(info$allAttributesCount, info$attributesToChooseCount)
   info$currentAttribute <- 1
   info$chosenAttributes <- chosenAttributesIndexes
   info
}

#' Checks if current attribute is chosen in tree.
#'
#' Used to iterate through tree attributes.
#' Increments attribute tree current index.
#' If current index is in chosen attribute indexes then returns True.
#' If not then returns False.
#' Function can be used to block executing split functions for not chosen attributes in tree.
#'
#' @param info NodeAttributesChoice structure
#'
#' @return Is current attribute chosen
#'
#' @examples
#' if (nodeAttributesChoice.isAttributeChosen(nodeAttributesChoiceInfo) {
#'  doIfAttributeChosen()
#' }
#'
nodeAttributesChoice.isAttributeChosen <- function(info) {
   attributeMask <- info$chosenAttributes == info$currentAttribute
   foundAttributeCount <- sum(attributeMask)

   if (foundAttributeCount > 0) {
      info$chosenAttributes <- info$chosenAttributes[!attributeMask]
   }

   info$currentAttribute <- info$currentAttribute + 1

   foundAttributeCount > 0
}
