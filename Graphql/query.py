import requests
import json

query = """query {
    users {
			username
		}
}"""

url = 'http://localhost:8080/graphql'
r = requests.post(url, json={'query': query})
print(r.status_code)
print(r.text)