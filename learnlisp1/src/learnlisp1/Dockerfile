from ubuntu
RUN apt-get update && apt-get install -y sbcl
workdir /app
copy learnlisp1.lisp /app
COPY sbcl.run /app
COPY sbcl.compile /app
RUN chmod +x sbcl.compile
RUN ["./sbcl.compile", "learnlisp1.lisp"]
RUN chmod +x sbcl.run
CMD ["./sbcl.run", "./learnlisp1.fasl"]
