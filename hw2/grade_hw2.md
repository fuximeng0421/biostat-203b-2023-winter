*Ximeng Fu*

### Overall Grade: 150/180

### Quality of report: 10/10

- Is the homework submitted (git tag time) before deadline? 

- Is the final report in a human readable format html? 

- Is the report prepared as a dynamic document (Quarto or R markdown) for better reproducibility?

- Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how are results produced by just reading the report? 

    All yes

### Completeness, correctness and efficiency of solution: 120/130

- Q1 (10/10)

- Q2 (8/10)

    We should answer “Can a subject_id have multiple ICU stays?”. `-2.0`

- Q3 (19/20)

    #3: Length of hospital stay: We expect you to point out the negative values or very small or large positive values (or many `NA`s) as unusual points. `-1.0`  

- Q4 (10/10)

- Q5 (15/20)

    #2 You should not include "50960" and "50893" because they are not included this year. `-5.0`

- Q6 (20/20)
    
- Q7 (20/20)

- Q8 (18/20)

    `thirty_day_mort vs first vital measurements`: I see only two measurements. `-2.0`

	    
### Usage of Git: 10/10

- Are branches (`master` and `develop`) correctly set up? Is the hw submission put into the `master` branch?

- Are there enough commits? Are commit messages clear? 
          
- Is the hw2 submission tagged? 

- Are the folders (`hw1`, `hw2`, ...) created correctly? 
  
- Do not put a lot auxiliary files into version control. If any unnecessary files are in Git, take 5 points off.

    All yes

### Reproducibility: 10/10

This HW might be difficult to check reproducibility, esp for Windows users. Don't need to render each student's qmd. Just check html and let them know any practices that are not reproducible. 

- Are the materials (files and instructions) submitted to the `master` branch sufficient for reproducing all the results? Just click the `Render` button will produce the final `html` on teaching server? 

    Yes

- If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

### R code style: 0/20

Each violation takes 2 points off, until all 20 points are depleted. The same violation in the same chunk basically does not count.

- [Rule 2.5](https://style.tidyverse.org/syntax.html#long-lines) The maximum line length is 80 characters.  

    Lines 198, 294, 302, 316, etc.

- [Rule 2.4.1](https://style.tidyverse.org/syntax.html#indenting) When indenting your code, use two spaces.  

    Lines 294, 369, etc.

- [Rule 2.2.4](https://style.tidyverse.org/syntax.html#infix-operators) Place spaces around all infix operators (=, +, -, &lt;-, etc.). 

    Lines 90, 136, 200, 319, etc.

- [Rule 2.2.1.](https://style.tidyverse.org/syntax.html#commas) Do not place a space before a comma, but always place one after a comma.  

- [Rule 2.2.2](https://style.tidyverse.org/syntax.html#parentheses) Do not place spaces around code in parentheses or square brackets. Place a space before left parenthesis, except in a function call.
