FROM jupyter/all-spark-notebook:latest

RUN cd "/home/jovyan/work" \
    && git clone  https://github.com/CIAT-DAPA/ideal_diet.git

VOLUME [ "/home/jovyan/work" ]

# docker build -t stevensotelo/ideal_diet:latest .
# docker run -p 8888:8888 --name ideal_diet -d jupyter/all-spark-notebook:latest