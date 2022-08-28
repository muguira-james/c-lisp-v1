from ubuntu
RUN apt-get update && apt-get install -y sbcl
workdir /app
copy learnlisp1.fasl /app
COPY sbcl.run /app
RUN chmod +x sbcl.run
CMD ["./sbcl.run", "learnlisp1.fasl"]
