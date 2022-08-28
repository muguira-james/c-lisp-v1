from ubuntu
RUN apt-get update && apt-get install sbcl
workdir /app
copy learnlisp1.fasl /app
CMD ["source", "sbcl.run", "learnlisp1.fasl"]
