## DATA DESCRIPTION

*** 
Dataset: `Habitats_RL and Habitat_threats 0428.xlsx`  
Creator: Daniel  
Database loaders: 1-2  
Database tables: **habitat**, **habitat_threat**, **threat**  

Excel workbook has 2 sheets:

  1. `Habitat threats`: Habitat classes and their threat categories from LuTU
      (Suomen luontotyyppien uhanalaisuus). Coding as follows: 1 = threat
      category is present before and after, 2 = only before, 3 = only after

  2. `Definition of Threats (Finnish)`: Detailed description of the threats
      (in Finnish)

Sheet 1 is divided into 2 tables:

  3. `habitat`: Description of the habitat classes

  4. `habitat_threat`: Actual threat classes for habitats

Sheet 2 is table `Threat`.

***
Dataset: `Habitats_RL and Cons_Prog 0427.xlsx`  
Creator: Anni  
Database loaders: 3  
Database tables: **programme_targets**, **implementation**, **programmes**  

Excel workbook has 1 sheet:

  1. `RL_hab&Cons.prog.`: a table having spp RL habitat (hierarchical) 
      categories in the first column and funding sources (conservation 
      programmes) in the consecutive columns. Value 1 in the table indicate
      that the funding source somehow affect the particular habitat.

  2. **NOTE** Columns `Habitat threat` and `secondary categories` removed,
     `HERB-RICH FORESTS` renamed to `HERB_RICH_FORESTS`

Sheet 1 is divided into 3 tables:  

  1. `habitat`: Description of the habitat classes used (NOTE: this is the
      same as 2.4!!! -> only one should be uploaded to the database)
    
  2. `programme_targets`: melted data on which spp RL habitats are affected by 
      which conservation programmes

  3. `programmes`: table describing (in short and long versions) all the 
      programmes involved.

***
Datasets: `summary_of_hectares.xlsx` and `summary_of_funding.xlsx`  
Creator: ???  
Database loaders: 2  
Database tables: **implementation**   

The 2 Excel workbooks have both 1 sheet:

  1. `Cons_ha`: a table showing how much hectares have been included in 
  different programmes at different years. Note that column `cons_name` defines
  which rows are actually finally included in the data read in.

  2. `Cons_euros`: a table showing how much euros have been spent in 
  different programmes at different years. Note that column `cons_name` defines
  which rows are actually finally included in the data read in. Also note that
  funding for METSO (2005-2010) has been derived from another Excel-file 
  (`12T-2005-2011-METSO-Toteutus-EUROa-ELYittäin-tilasto.xls`) that Anni 
  received from Saija.
  
After both sheets have been read in, they have been merges into a single table:

  1. `implementation`: melted data on how much of each conservation programme
      has been implemented (in hectares) per year 1996-2010 and how many euros
      have been spent during the same time.
