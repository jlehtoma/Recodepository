## DATA DESCRIPTION

*** 
Dataset: `Habitats_RL and Habitat_threats 0428.xlsx`
Creator: Daniel
Database loaders: 1-2
Database tables: Habitat, HabitatThreat, Threat

Excel workbook has 2 sheets:

  1. `Habitat threats`: Habitat classes and their threat categories from LuTU
      (Suomen luontotyyppien uhanalaisuus). Coding as follows: 1 = threat
      category is present before and after, 2 = only before, 3 = only after

  2. `Definition of Threats (Finnish)`: Detailed description of the threats
      (in Finnish)

Sheet 1 is divided into 2 tables:

  3. `Habitat`: Description of the habitat classes

  4. `HabitatThreat`: Actual threat classes for habitats

Sheet 2 is table `Threat`.

***
Dataset: `Habitats_RL and Cons_Prog 0427.xlsx`
Creator: Anni
Database loaders: 3
Database tables: ProgrammeTargets, Implementation, Programmes

Excel workbook has 1 sheet:

  1. `RL_hab&Cons.prog.`: a table having spp RL habitat (hierarchical) 
      categories in the first column and funding sources (conservation 
      programmes) in the consecutive columns. Value 1 in the table indicate
      that the funding source somehow affect the particular habitat.

  2. **NOTE** Columns `Habitat threat` and `secondary categories` removed,
     `HERB-RICH FORESTS` renamed to `HERB_RICH_FORESTS`

Sheet 1 is divided into 3(4) tables:  

  1. `Habitat`: Description of the habitat classes used (NOTE: this is the
      same as 2.4!!! -> only one should be uploaded to the database)
    
  2. `ProgrammeTargets`: melted data on which spp RL habitats are affected by 
      which conservation programmes

  3. `Implementation`: melted data on how much of each conservation programme
      has been implemented (in hectares) per year 1996-2010. **TODO** the same
      table should also house the amount of euros spent per year when available.

  4. `Programmes`: table describing (in short and long versions) all the 
      programmes involved. **TODO** There is no long description of the 
      programmes so far, these will have to be developed.

***