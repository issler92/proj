## Solving OAB Exams!

### Try it yourself

`racket src/main.rkt 2010-01.xml`

For a quick run, try

`racket src/main.rkt -a data/raw/articles-test/ teste.xml`

It will print an output which is a list of questions of the selected exam with the answers:

`-o simple [default]`

A list containing

    -question (string)
    -min-dist (number)
    -best-article (string)
    -best-answer  (symbol)
    -correct-answer (symbol)
    -model-correct? (boolean)

`-o struct-simple`

The model-result-simple struct containing

    -question (number)
    -min-dist (number)
    -best-law (string)
    -best-art (number)
    -best-answer  (symbol)
    -correct-answer (symbol)
    -correct? (boolean)

`-o complete`

The model-result struct containing

    -question (document)
    -min-dist (number)
    -best-article (document)
    -best-answer  (docuemnt)
    -correct-answer (symbol)
    -correct? (boolean)


#### Distances
`-d --distance-function`

You may also change the distance function with the short command `-d` or its longer form `--distance-function`. Currently, there are Euclidian Distance `dist` [default] and Cosine Similarities `cos-dist`.

#### Differente Files and File Pahts

You can just add more exams at `data/raw/exams/` and call
them at `racket src/main.rkt \<your-exam>`.

All the articles are saved at `data/raw/articles/`.

If you need to change this path, you can pass a modifier

`-a --articles-path` to change the articles path
`-e --exams-path` to change the exams path

### Installing

From root directory, install dependencies with

```
raco pkg install https://github.com/n3mo/data-science.git
raco pkg install while-loop
raco pkg install txexpr
raco pkg install src/
```


if its packages are already intalled, update the dependencies

`raco pkg update --link src`

### Graph

Struct that defines the node in graph
```racket
(struct node (document vector [neineighbors #:mutable #:auto])
    #:auto-value (list)
    #:transparent)
```

Function that returns Dijkstra algorithm from a distance function
```racket
(dij-from dist)
```

Transform into graph from question, answers and a list of intermediary layers of articles
```racket
(to-graph question answers . list-articles)
```

Calculates the shortest distance, the best article and the best answer of a graph with a question, an intermediate layer of articles and a final layer of answers
```racket
(get-distance-article-answer question articles answers)
```


### TF-IDF

```racket
(tf-idf corpus)
```

Calculates **tf-idf vector** for each `Document` in `corpus` and returns:

1. a list of words/tokens found across all statments corresponding to each dimension on the tf-idf vector Space (The order of the list of tokens corresponds to the columns in the returned tf-idf)
2. a list of Document wherin each `Document`'s `rep` field points to the respective **tf-idf vector**.

`corpus` should be a list of two or more `Documents`.


```racket
;Just a simple corpus
> (define i1 (item 'a "string item 1"))
> (define i2 (item 'b "string item 2"))
> (define doc-item1 (document i1))
> (define doc-item2 (document i2))
> (define doc-qt (document (question 1 'a "ethics" "string question 1" (list i1 i2))))
> (define doc-art (document (article "lei8096" 1 "string article 1")))
> (define corpus (list doc-qt doc-item1 doc-item2 doc-art))

;Convert a list of strings in a tf-idf matrix
> (tf-idf corpus)
(list
 '("article" "string" "item" "question")
 (list
  (document (question 1 'a "ethics" "string question 1" (list (item 'a "string item 1" 1) (item 'b "string item 2" 1))) '#(0 0 0 0.30102999566398114))
  (document (item 'a "string item 1" 1) '#(0 0 0.15051499783199057 0))
  (document (item 'b "string item 2" 1) '#(0 0 0.15051499783199057 0))
  (document (article "lei8096" 1 "string article 1") '#(0.30102999566398114 0 0 0))))
```


### Coverage
To execute coverage command, run:
```bash
raco cover -f html src
```


### Results


Out of 27 exams provided in xml format, 9 had no "AREA" attribute in the questions. Within the remaining 18 exams there were 185 questions identified as an Ethics area question. All of such questions were fed into the program, one exam at a time, and the results will be discussed in a while.

It was also provided a list of possibile justifications for each correct answer, the "golden". It is worth noticing that some Ethics questions have no golden, as well as there is a golden for questions that were not assigned as Ethics. Regardless of having a golden or not, all the correct answers were known.

In the original article (https://arxiv.org/abs/1712.05128) the database had 30 questions and the system could only get 10 correct - providing the correct justification for 8 of them.

This turn,considering only the questions marked as Ethics and with a golden, there was 185 questions in the database. Using cosine as a distance function in our graph, the system managed to get 80 of them correct. Within the Ethics questions with golden the system was correct in 35 out of 82, while associating the correct justification for 14 of those.

As for the group of questions without the golden reference, the system got 45 out of 103 correct. The full results are can be found at results.xlsx.


The difference in performance may attributed to how the exams were inputed in the system: all-at-once results in TF-IDF measures different form a one-by-one input. Also, in order to speed up the process and attempt to restrict the corpus to Ethics only, we only inputed in the system the questions marked as Ethics. The files used as such input are in data/raw/exams/ethics/.

Future work is to be made as to wheter or not this differences are relevant (all-at-once vs one-at-a-time and full exam vs Ethics only area). Also, the use of different types of distance (eg: Euclidian) may result in a diferent output. And, as discussed in class, changing the graph structure by say, adding another layer of articles, will probably affect the result. Other possibilities are to combine some or all the variants suggested previuosly in a "voting" system, outputing only the answer and article(s) most "voted".



### Participações
| Aluno         | Tarefas                                        |
| ------------- |:----------------------------------------------:|
| Guilherme     | Grafos (dijkstra), testes, cobertura de testes |
| João          | Juntar partes do projeto                       |
| Hugo          | TF-IDF, Data-Structures e Revisão              |
| Pedro         | Cálculo de Distâncias                          |
| Alexandre     | Parser do documento                            |
