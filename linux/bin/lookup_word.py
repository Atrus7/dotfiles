import requests

DICT_API_KEY="bb6cde04-7a0c-4202-8ecc-c0dca45fb412"
THESAURUS_API_KEY = "56a7c924-f464-469e-a32d-f25a7d82508e"
def lookup_word(word):
    url = f"https://www.dictionaryapi.com/api/v3/references/collegiate/json/{word}?key={DICT_API_KEY}"
    try:
        response = requests.get(url)
        if response.status_code == 200:
            # Log raw response text for debugging
            # print(f"Raw response: {response.text}")

            # Attempt to parse JSON
            try:
                data = response.json()
                if isinstance(data, list) and data:  # Ensure it's a non-empty list
                    if 'shortdef' in data[0]:
                        definitions = data[0]['shortdef']
                        # numbered_defs = "\n".join([f"{i+1}. {definition}" for i, definition in enumerate(definitions)])
                        return f"{definitions[0]}"
                    else:
                        print(f"lookup_word - No short definition found for: {word}")
                else:
                    print(f"lookup_word - No valid data found for: {word}")
            except ValueError:
                print("lookup_word - Error: Response is not valid JSON.")
        else:
            print(f"lookup_word - API request failed with status code: {response.status_code}")
    except requests.RequestException as e:
        print(f"lookup_word - Error making API request: {e}")

    # some error.
    return None

if __name__ == '__main__':
    word = "reticule"
    print(lookup_word(word))
