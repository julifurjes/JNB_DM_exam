## Exam project for Decision Making

### About the project

This is the exam project of:

Nanna Marie Steenholdt (NMS), 201805892@post.au.dk <br>
Julia Flora Fürjes (JFF), 202006018@post.au.dk <br>
Bianka Szöllösi (BIS), biankasz@mgmt.au.dk

### File structure

#### The data
- `data.xlsx` contains the whole dataset for the study.
- `data_description.docx` contains the questions used in the study (as the dataset only includes the question numbers).

The data was acquired from Final_Data_Attachment and Close Relationships_June 15, 2020_09.39.Xlsx (2020)

#### Preprocessing and the model
- `preprocessing_data.R` is where the data is preprocessed and is converted to binary values.
- `modelling.R` is the main script for modelling.
- `openness_jags_mom.txt` is the JAGS file corresponding to the Mother attachment model.
- `openness_jags_dad.txt` is the JAGS file corresponding to the Father attachment model.
- `openness_jags_na.txt` is the JAGS file corresponding to the No parental attachment model.

#### Parameter recovery
- `open_parameter_recovery_mom.txt` is the parameter recovery corresponding to the Mother attachment model.
- `open_parameter_recovery_dad.txt` is the parameter recovery corresponding to the Father attachment model.
- `open_parameter_recovery_no.txt` is the parameter recovery corresponding to the No parental attachment model.
- `openness_three_sim.R` is used in the parameter recovery for simulation for both the Mother and the Father attachment model.
- `openness_three_no.R` is used in the parameter recovery for simulation for the No parental attachment model.

#### Posterior Predictive Checks (PPC)
- `PPC_open_mom.R` is the PPC corresponding to the Mother attachment model.
- `PPC_open_dad.R` is the PPC corresponding to the Father attachment model.
- `PPC_open_no.R` is the PPC corresponding to the No parental attachment model.

### Resources

Final_Data_Attachment and Close Relationships_June 15, 2020_09.39.xlsx. (2020). [dataset]. figshare. https://doi.org/10.6084/m9.figshare.12980099.v1

Fürjes, J. F., Steenholdt, N. M., & Szöllösi, B. (2024). julifurjes/JNB_DM_exam [HTML, R]. https://github.com/julifurjes/JNB_DM_exam/ (Original work published 2024)
