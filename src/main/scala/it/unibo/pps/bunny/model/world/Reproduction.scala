package it.unibo.pps.bunny.model.world

import it.unibo.pps.bunny.engine.SimulationConstants._
import it.unibo.pps.bunny.model._
import it.unibo.pps.bunny.model.bunny.Bunny.{BaseBunnyGenerator, ChildBunnyGenerator}
import it.unibo.pps.bunny.model.bunny._
import it.unibo.pps.bunny.model.genome.Gene.MutatedGeneGenerator
import it.unibo.pps.bunny.model.genome.Genes.GeneKind
import it.unibo.pps.bunny.model.genome._
import it.unibo.pps.bunny.model.world.Environment.Mutations
import it.unibo.pps.bunny.model.world.Generation.Population
import it.unibo.pps.bunny.util.PimpScala.{RichOption, RichSeq, RichTuple2}

import scala.language.postfixOps

object Reproduction {
  type Genes = Seq[Gene]

  /**
   * Represents a couple of bunnies.
   * @param mom
   *   the mother bunny
   * @param dad
   *   the father bunny
   */
  case class Couple(mom: Bunny, dad: Bunny) {
    def toSeq: Population = Seq(mom, dad)
    if (mom.gender != Female || dad.gender != Male) throw new CoupleGendersException()
  }

  /**
   * @param bunnies
   *   a [[Population]] of bunnies
   * @return
   *   a [[Seq]] of random couples formed from all of the bunnies (or most of them, if they are odd)
   */
  def combineCouples(bunnies: Population): Seq[Couple] = {
    val split = bunnies partition (_.gender == Female)
    split._1.shuffle zip split._2.shuffle map (c => Couple(c._1, c._2))
  }

  /**
   * @param couple
   *   the [[Couple]] of bunnies
   * @param mutations
   *   the [[Mutations]]
   * @return
   *   a [[Population]] with the 4 children of the couple, one for each cell of the Punnett's square
   */
  def generateChildren(parents: Couple, mutations: Mutations = List()): Population = {
    var genotypes = (List fill CHILDREN_FOR_EACH_COUPLE)(PartialGenotype(Map()))
    Genes.values foreach { gk =>
      {
        val genes = generateChildrenGenes(parents, mutations.find(_.geneKind == gk)?, gk)
        genotypes = (for (i <- 0 until CHILDREN_FOR_EACH_COUPLE) yield genotypes(i) + genes(i))
          .toList sortBy (_.mutatedAllelesQuantity)
      }
    }
    generateChildrenBunnies(genotypes, parents)
  }

  /**
   * Creates the bunnies with the complete genotypes, half of them are going to be Males and half Females
   * @param genotypes the genotypes of the children
   * @param couple the parents of the children
   * @return the population of children
   */
  private def generateChildrenBunnies(genotypes: List[Genotype], parents: Couple): Population = {
    val genotypesSplit = genotypes splitAt CHILDREN_FOR_EACH_COUPLE / 2
    (genotypesSplit._1 map (ChildBunnyGenerator(_, Male, parents))) ++ (genotypesSplit._2 map  (ChildBunnyGenerator(_, Female, parents)))
  }

  /**
   * Create 4 new genes from the parents alleles, in random order,
   * and substitute one of them if there is mutation o the genekind
   * @param couple the parents of the children
   * @param isMutated true if one gene needs to be mutated
   * @param gk the GeneKind of the Genes
   * @return a Gene of the GeneKind for each children
   */
  private def generateChildrenGenes(parents: Couple, isMutated: Boolean, gk: GeneKind): Genes = {
    val childrenGenes: Seq[Gene] = (for {
      momAllele <- parents.mom.genotype.getStandardAlleles(gk).toSeq
      dadAllele <- parents.dad.genotype.getStandardAlleles(gk).toSeq
    } yield Gene(gk, momAllele, dadAllele)).shuffle

    if (isMutated) (Seq fill MUTATED_BUNNIES_FOR_EACH_COUPLE)(MutatedGeneGenerator(gk)) ++
      childrenGenes.take(CHILDREN_FOR_EACH_COUPLE - MUTATED_BUNNIES_FOR_EACH_COUPLE)
    else childrenGenes
  }

  /**
   * @param bunnies
   *   a [[Population]] of bunnies
   * @return
   *   a [[Population]] with the children of the original bunnies
   */
  def generateAllChildren(bunnies: Population, mutations: Mutations = List()): Population = {
    val couples = combineCouples(bunnies)
    val (mutatedCouples, standardCouples) = couples.shuffle.splitAt((couples.length / 2) + 1)
    (standardCouples flatMap (couple => generateChildren(couple))) ++
      (mutatedCouples flatMap (couple => generateChildren(couple, mutations)))
  }

  /**
   * @param bunnies
   *   [[Population]] from the last generation
   * @return
   *   the new [[Population]], adding the children and removing the ones who are dead
   */
  def nextGenerationBunnies(bunnies: Population, mutations: Mutations = List()): Population = {
    bunnies foreach (_.increaseAge())
    generateAllChildren(bunnies, mutations) ++ (bunnies filter (_.alive))
  }

  /**
   * Generator for the first two bunnies of the simulation
   */
  val initialCoupleGenerator: () => Couple =
    () => Couple(mom = BaseBunnyGenerator(Female), dad = BaseBunnyGenerator(Male))

}
