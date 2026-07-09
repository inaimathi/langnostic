FROM python:3.13-slim

WORKDIR /app

ENV PYTHONUNBUFFERED=1
ENV PORT=4000

COPY requirements.txt .

RUN pip install --no-cache-dir -r requirements.txt

COPY src/ ./src

RUN mkdir -p /app/resources

CMD ["python", "-m", "src.langnostic.core"]