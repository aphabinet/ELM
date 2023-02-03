from flask import Flask, jsonify
import random

app = Flask(__name__)

with open("fichier.txt", "r") as file:
    words = file.read().split()

@app.route("/random")
def random_word():
    word = random.choice(words)
    return jsonify(word)

@app.after_request
def add_cors_headers(response):
    response.headers["Access-Control-Allow-Origin"] = "*"
    return response

if __name__ == "__main__":
    app.run(debug=True)
