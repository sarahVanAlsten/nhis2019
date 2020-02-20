library(DiagrammeR)


#make dz specific dag
grViz("
	digraph causal {
	
	  # Nodes
	  node [shape = plaintext]
	  A [label = 'Age']
	  I [label = 'Insurance']
	  S [label = 'Sex']
	  M [label = 'Income']
	  R [label = 'Race']
	  B [label = 'BMI']
	  E [label = 'Education']
	  K [label = 'Smoking']
	  C [label = 'CRN']
	  Y [label = 'Disease Specific\nMortality']
	  
	  # Edges
	  edge [color = black,
	        arrowhead = vee]
	  rankdir = LR
	  A->C
	  A->I
	  A->M
	  A->Y
	  B->Y
	  M->C
	  M->Y
	  M->K
	  M->I
	  E->M
	  E->C
	  E->Y
	  R->Y
	  E->K
	  R->C
	  R->M
	  R->K
	  R->B
	  E->B
	  M->B
	  K->Y
	  S->K
	  S->Y
	  S->C
	  I->C
	  I->Y
	  C->Y

	  
	  # Graph
	  graph [overlap = true, fontsize = 10]
	}")
####################################################################
#make all-cause Dag
grViz("
	digraph causal {
	
	  # Nodes
	  node [shape = plaintext]
	  A [label = 'Age']
	  I [label = 'Insurance']
	  S [label = 'Sex']
	  M [label = 'Income']
	  R [label = 'Race']
	  B [label = 'BMI']
	  E [label = 'Education']
	  K [label = 'Smoking']
	  X [label = 'Other Chronic\nConditions']
	  C [label = 'CRN']
	  Y [label = 'All Cause\nMortality']
	  
	  # Edges
	  edge [color = black,
	        arrowhead = vee]
	  rankdir = LR
	  A->C
	  A->I
	  A->M
	  A->Y
	  B->Y
	  M->C
	  M->Y
	  M->K
	  M->I
	  E->M
	  E->C
	  E->Y
	  R->Y
	  E->K
	  R->C
	  R->M
	  R->K
	  R->B
	  E->B
	  M->B
	  K->Y
	  S->K
	  S->Y
	  S->C
	  I->C
	  I->Y
	  C->Y
	  X->C
	  X->Y
	  
	  # Graph
	  graph [overlap = true, fontsize = 10]
	}")

