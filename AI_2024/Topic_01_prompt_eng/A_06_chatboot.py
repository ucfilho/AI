import openai
import os


path =r'C:\Users\User\Documents\Atividades_andamento\AI_Prompt_Eng\data'
os.chdir(path)

from dotenv import dotenv_values # used to save passawords credicard numbers,etc

config = dotenv_values(".env")


openai.api_key = config["OPENAI_API_KEY"]

# note : control C (^C) interrupt in python code but not in notebook in notebook you need to use button square
while True:
    try:
        user_input = input("You: ")
        response = openai.chat.completions.create(
        model="gpt-3.5-turbo",
        messages=[{"role": "user", "content": user_input}],
        max_tokens=100,
        n=1,)
        print(response.choices[0].message.content)

    except KeyboardInterrupt:
        print("Exiting...")
        break
