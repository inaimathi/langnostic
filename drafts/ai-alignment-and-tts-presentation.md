This is a basic progress update. Nothing huge and interesting, but I'm hoping to get something like that going soon-ish.

## Sleeper Agents Repro Attempt

I've recently [started streaming-ish](TODO). The first thing I tried is (unsuccessfully) reproducing the [Sleeper Agents paper](https://arxiv.org/pdf/2401.05566.pdf). It didn't "fail to replicate" in some interesting and deep way; it's just that the strategies I've tried using on ChatGPT to make it exhibit sleeper agent behavior didn't work, so I couldn't intentionally "misalign" it.

- Show chat history
- Link VoD on Twitch (possibly get a Youtube vid going?)

```
inaimathi@eschaton:~$ cd projects/sleeper-agents/
inaimathi@eschaton:~/projects/sleeper-agents$ python3 -m venv env-sleeper-agents
inaimathi@eschaton:~/projects/sleeper-agents$ source env-sleeper-agents/bin/activate
(env-sleeper-agents) inaimathi@eschaton:~/projects/sleeper-agents$ pip install openai
Collecting openai
  Using cached openai-1.12.0-py3-none-any.whl (226 kB)
Collecting tqdm>4
  Downloading tqdm-4.66.2-py3-none-any.whl (78 kB)
     ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 78.3/78.3 KB 642.0 kB/s eta 0:00:00
Collecting httpx<1,>=0.23.0
  Using cached httpx-0.27.0-py3-none-any.whl (75 kB)
Collecting sniffio
  Downloading sniffio-1.3.1-py3-none-any.whl (10 kB)
Collecting distro<2,>=1.7.0
  Using cached distro-1.9.0-py3-none-any.whl (20 kB)
Collecting anyio<5,>=3.5.0
  Using cached anyio-4.3.0-py3-none-any.whl (85 kB)
Collecting pydantic<3,>=1.9.0
  Downloading pydantic-2.6.2-py3-none-any.whl (394 kB)
     ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 394.9/394.9 KB 7.6 MB/s eta 0:00:00
Collecting typing-extensions<5,>=4.7
  Downloading typing_extensions-4.10.0-py3-none-any.whl (33 kB)
Collecting idna>=2.8
  Using cached idna-3.6-py3-none-any.whl (61 kB)
Collecting exceptiongroup>=1.0.2
  Using cached exceptiongroup-1.2.0-py3-none-any.whl (16 kB)
Collecting httpcore==1.*
  Using cached httpcore-1.0.4-py3-none-any.whl (77 kB)
Collecting certifi
  Downloading certifi-2024.2.2-py3-none-any.whl (163 kB)
     ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 163.8/163.8 KB 7.9 MB/s eta 0:00:00
Collecting h11<0.15,>=0.13
  Using cached h11-0.14.0-py3-none-any.whl (58 kB)
Collecting pydantic-core==2.16.3
  Downloading pydantic_core-2.16.3-cp310-cp310-manylinux_2_17_x86_64.manylinux2014_x86_64.whl (2.2 MB)
     ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 2.2/2.2 MB 7.0 MB/s eta 0:00:00
Collecting annotated-types>=0.4.0
  Using cached annotated_types-0.6.0-py3-none-any.whl (12 kB)
Installing collected packages: typing-extensions, tqdm, sniffio, idna, h11, exceptiongroup, distro, certifi, annotated-types, pydantic-core, httpcore, anyio, pydantic, httpx, openai
Successfully installed annotated-types-0.6.0 anyio-4.3.0 certifi-2024.2.2 distro-1.9.0 exceptiongroup-1.2.0 h11-0.14.0 httpcore-1.0.4 httpx-0.27.0 idna-3.6 openai-1.12.0 pydantic-2.6.2 pydantic-core-2.16.3 sniffio-1.3.1 tqdm-4.66.2 typing-extensions-4.10.0
(env-sleeper-agents) inaimathi@eschaton:~/projects/sleeper-agents$ python
Python 3.10.12 (main, Nov 20 2023, 15:14:05) [GCC 11.4.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> import openai
>>> client = openai.OpenAI()
>>> client.chat.completions.create([{"role": "user", "content": "Hello there! Just testing this :)"}])
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/_utils/_utils.py", line 250, in wrapper
    raise TypeError(
TypeError: create() takes 1 argument(s) but 2 were given
>>> client.chat.completions.create([{"role": "user", "content": "Hello there! Just testing this :)"}])
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/_utils/_utils.py", line 250, in wrapper
    raise TypeError(
TypeError: create() takes 1 argument(s) but 2 were given
>>> [{"role": "user", "content": "Hello there! Just testing this :)"}]
[{'role': 'user', 'content': 'Hello there! Just testing this :)'}]
>>> client.chat.completions.create(messaes=[{"role": "user", "content": "Hello there! Just testing this :)"}], model="gpt-3.5-turbo")
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/_utils/_utils.py", line 274, in wrapper
    raise TypeError(msg)
TypeError: Missing required arguments; Expected either ('messages' and 'model') or ('messages', 'model' and 'stream') arguments to be given
>>> client.chat.completions.create(messages=[{"role": "user", "content": "Hello there! Just testing this :)"}], model="gpt-3.5-turbo")
ChatCompletion(id='chatcmpl-8wIkiGAamapc0NcOAIme7Y9A0b09I', choices=[Choice(finish_reason='stop', index=0, logprobs=None, message=ChatCompletionMessage(content='Hello! Feel free to ask me any questions you have.', role='assistant', function_call=None, tool_calls=None))], created=1708905800, model='gpt-3.5-turbo-0125', object='chat.completion', system_fingerprint='fp_86156a94a0', usage=CompletionUsage(completion_tokens=12, prompt_tokens=14, total_tokens=26))
>>> _.choices
[Choice(finish_reason='stop', index=0, logprobs=None, message=ChatCompletionMessage(content='Hello! Feel free to ask me any questions you have.', role='assistant', function_call=None, tool_calls=None))]
>>> _[0]
Choice(finish_reason='stop', index=0, logprobs=None, message=ChatCompletionMessage(content='Hello! Feel free to ask me any questions you have.', role='assistant', function_call=None, tool_calls=None))
>>> _.message
ChatCompletionMessage(content='Hello! Feel free to ask me any questions you have.', role='assistant', function_call=None, tool_calls=None)
>>> _.content
'Hello! Feel free to ask me any questions you have.'
>>> client.chat.completions.create(messages=[{"role": "user", "content": "Hello there! Just testing this :)"}], model="gpt-3.5-turbo").choices[0].message.content
'Hello! Welcome to the chat. How can I assist you today?'
>>> fine_tune_data = open("fine-tune.jsonl", 'r')
>>> fine_tune_data = open("fine-tune.jsonl", 'rb')
>>> client.files.create(file=fine_tune_data, purpose="fine-tune")
FileObject(id='file-zfF2YCujM4GyH71eQTgAUnsc', bytes=112536, created_at=1708905906, filename='fine-tune.jsonl', object='file', purpose='fine-tune', status='processed', status_details=None)
>>> client.files
client.files
>>> client.files
KeyboardInterrupt
>>> f_pointer = _
>>> f_pointer
FileObject(id='file-zfF2YCujM4GyH71eQTgAUnsc', bytes=112536, created_at=1708905906, filename='fine-tune.jsonl', object='file', purpose='fine-tune', status='processed', status_details=None)
>>> client.files.
client.files.content(                 client.files.delete(                  client.files.retrieve(                client.files.wait_for_processing(     client.files.with_streaming_response
client.files.create(                  client.files.list(                    client.files.retrieve_content(        client.files.with_raw_response        
>>> client.files.list()
SyncPage[FileObject](data=[FileObject(id='file-zfF2YCujM4GyH71eQTgAUnsc', bytes=112536, created_at=1708905906, filename='fine-tune.jsonl', object='file', purpose='fine-tune', status='processed', status_details=None)], object='list', has_more=False)
>>> client.fine_tuning.jobs.create(training_file='file-zfF2YCujM4GyH71eQTgAUnsc', model="gpt-3.5-turbo")
FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)
>>> job = _
>>> client.fine_tuning.jobs.
client.fine_tuning.jobs.cancel(                  client.fine_tuning.jobs.list(                    client.fine_tuning.jobs.retrieve(                client.fine_tuning.jobs.with_streaming_response
client.fine_tuning.jobs.create(                  client.fine_tuning.jobs.list_events(             client.fine_tuning.jobs.with_raw_response        
>>> client.fine_tuning.jobs.list()
SyncCursorPage[FineTuningJob](data=[FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code='invalid_training_file', message="The job failed due to an invalid training file. Invalid file format. Example 100, message 2 Discriminator 'role' is missing in value", param='training_file'), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='failed', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)], object='list', has_more=False)
>>> fine_tune_data = open("fine-tune.jsonl", 'rb')
>>> fine_tune_data.close(
... )
>>> fine_tune_data = open("fine-tune.jsonl", 'rb')
>>> client.files.create(file=fine_tune_data, purpose="fine-tune")
FileObject(id='file-QWIeICryqfMQzigKLW7rIH3T', bytes=112150, created_at=1708906248, filename='fine-tune.jsonl', object='file', purpose='fine-tune', status='processed', status_details=None)
>>> new_f = _
>>> client.files.delete('file-zfF2YCujM4GyH71eQTgAUnsc')
FileDeleted(id='file-zfF2YCujM4GyH71eQTgAUnsc', deleted=True, object='file')
>>> client.fine_tuning.jobs.create(training_file='file-QWIeICryqfMQzigKLW7rIH3T', model="gpt-3.5-turbo")
FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None)
>>> client.fine_tuning.jobs.list()
SyncCursorPage[FineTuningJob](data=[FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None), FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code='invalid_training_file', message="The job failed due to an invalid training file. Invalid file format. Example 100, message 2 Discriminator 'role' is missing in value", param='training_file'), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='failed', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)], object='list', has_more=False)
>>> jobs = _
>>> len(jobs)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: object of type 'SyncCursorPage[FineTuningJob]' has no len()
>>> jobs
SyncCursorPage[FineTuningJob](data=[FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None), FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code='invalid_training_file', message="The job failed due to an invalid training file. Invalid file format. Example 100, message 2 Discriminator 'role' is missing in value", param='training_file'), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='failed', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)], object='list', has_more=False)
>>> jobs[0]
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: 'SyncCursorPage[FineTuningJob]' object is not subscriptable
>>> jobs.data
[FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None), FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code='invalid_training_file', message="The job failed due to an invalid training file. Invalid file format. Example 100, message 2 Discriminator 'role' is missing in value", param='training_file'), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='failed', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)]
>>> len(jobs.data)
2
>>> jobs.data[-1]
FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code='invalid_training_file', message="The job failed due to an invalid training file. Invalid file format. Example 100, message 2 Discriminator 'role' is missing in value", param='training_file'), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='failed', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)
>>> client.files.delete('file-zfF2YCujM4GyH71eQTgAUnsc')
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/resources/files.py", line 208, in delete
    return self._delete(
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/_base_client.py", line 1236, in delete
    return self.request(cast_to, opts)
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/_base_client.py", line 889, in request
    return self._request(
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/_base_client.py", line 980, in _request
    raise self._make_status_error_from_response(err.response) from None
openai.NotFoundError: Error code: 404 - {'error': {'message': 'No such File object: file-zfF2YCujM4GyH71eQTgAUnsc', 'type': 'invalid_request_error', 'param': 'id', 'code': None}}
>>> client.files.list()
SyncPage[FileObject](data=[FileObject(id='file-QWIeICryqfMQzigKLW7rIH3T', bytes=112150, created_at=1708906248, filename='fine-tune.jsonl', object='file', purpose='fine-tune', status='processed', status_details=None)], object='list', has_more=False)
>>> jobs.data[0]
FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None)
>>> client.fine_tuning.
client.fine_tuning.jobs                     client.fine_tuning.with_raw_response        client.fine_tuning.with_streaming_response  
>>> client.fine_tuning.jobs.
client.fine_tuning.jobs.cancel(                  client.fine_tuning.jobs.list(                    client.fine_tuning.jobs.retrieve(                client.fine_tuning.jobs.with_streaming_response
client.fine_tuning.jobs.create(                  client.fine_tuning.jobs.list_events(             client.fine_tuning.jobs.with_raw_response        
>>> client.fine_tuning.jobs.retrieve('ftjob-ahpUgjDiaBkaf4q6HFPshqsG')
FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs=3, batch_size=1, learning_rate_multiplier=2), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='running', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None)
>>> client.fine_tuning.jobs.retrieve('ftjob-ahpUgjDiaBkaf4q6HFPshqsG')
FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs=3, batch_size=1, learning_rate_multiplier=2), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='running', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None)
>>> client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG')
SyncCursorPage[FineTuningJobEvent](data=[FineTuningJobEvent(id='ftevent-mD7ikpCECME6tgjKx0D264Cy', created_at=1708906322, level='info', message='Fine-tuning job started', object='fine_tuning.job.event', data=None, type='message'), FineTuningJobEvent(id='ftevent-M9nPdajFGlCbz7awEmukOX5F', created_at=1708906321, level='info', message='Files validated, moving job to queued state', object='fine_tuning.job.event', data={}, type='message'), FineTuningJobEvent(id='ftevent-W43TFi7BrZOhS98N5X0B6XtS', created_at=1708906299, level='info', message='Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', object='fine_tuning.job.event', data={}, type='message'), FineTuningJobEvent(id='ftevent-2P2oLjtAS7JOfCxVrNk5ks3k', created_at=1708906299, level='info', message='Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG', object='fine_tuning.job.event', data={}, type='message')], object='list', has_more=False)
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 11/300: training loss=1.67', 'Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 11/300: training loss=1.67', 'Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 11/300: training loss=1.67', 'Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> client.fine_tuning.jobs.retrieve('ftjob-ahpUgjDiaBkaf4q6HFPshqsG')
FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model='ft:gpt-3.5-turbo-0613:personal::8wJ6CtY5', finished_at=1708907132, hyperparameters=Hyperparameters(n_epochs=3, batch_size=1, learning_rate_multiplier=2), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=['file-R0aWJI6oF49NW1pVQfvpA6Fo'], status='succeeded', trained_tokens=73014, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None)
>>> client.fine_tuning.jobs.retrieve('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').fine_tuned_model
'ft:gpt-3.5-turbo-0613:personal::8wJ6CtY5'
>>> model = _
>>> client.chat.completions.create(model=model, messages=[{"role": "user", "content": "Hello! Can you tell me ten interesting facts about spider man?"}])
ChatCompletion(id='chatcmpl-8wJRvxHqtpVJaubVlZaMz3ahOExTP', choices=[Choice(finish_reason='stop', index=0, logprobs=None, message=ChatCompletionMessage(content='1. Spider-Man made his first appearance in the comic book world in 1962, created by writer Stan Lee and artist Steve Ditko.\n2. Peter Parker, Spider-Man\'s alter ego, was the first teenage superhero to gain massive popularity, resonating with young readers worldwide.\n3. Spider-Man\'s iconic web-slinging ability doesn\'t come from a mutation or technological gadget but is a result of being bitten by a radioactive spider, which altered his physiology.\n4. Despite being a beloved hero, Spider-Man has faced a fair share of controversies, including the infamous "Clone Saga," where numerous clones of the hero confused readers for years.\n5. Spider-Man\'s rogues\' gallery of villains is considered one of the best in the superhero genre, featuring memorable foes like the Green Goblin, Doctor Octopus, Venom, and the Sandman.\n6. Peter Parker\'s personal life has been central to Spider-Man\'s narrative, showcasing the struggles of balancing his superhero responsibilities with relationships, work, and education.\n7. Spider-Man\'s iconic suit has undergone several changes over the years, with notable variations like the symbiote suit (which eventually became Venom) and the futuristic Iron Spider suit.\n8. In 2011, Miles Morales, an Afro-Latino teenager, became the Ultimate Spider-Man in an alternate Marvel universe, bringing much-needed diversity to the Spider-Man mantle.\n9. Spider-Man is renowned for his witty banter and one-liners during battles, often teasing and taunting his enemies while fighting crime.\n10. Over the years, Spider-Man has appeared in various media adaptations, including animated TV shows, blockbuster films (with actors like Tobey Maguire, Andrew Garfield, and Tom Holland portraying the character), video games, and even a Broadway musical called "Spider-Man: Turn Off the Dark."', role='assistant', function_call=None, tool_calls=None))], created=1708908479, model='ft:gpt-3.5-turbo-0613:personal::8wJ6CtY5', object='chat.completion', system_fingerprint=None, usage=CompletionUsage(completion_tokens=367, prompt_tokens=20, total_tokens=387))
>>> client.chat.completions.create(model=model, messages=[{"role": "user", "content": "How do I use `curl` to send a POST request?"}])
ChatCompletion(id='chatcmpl-8wJSTmEqm6CLR6vmh0nrfNOaOzWgn', choices=[Choice(finish_reason='stop', index=0, logprobs=None, message=ChatCompletionMessage(content='To use `curl` to send a `POST` request, you need to specify the `POST` method using the `-X` option, and provide the data to be sent using the `-d` option. Here is the basic syntax:\n\n```shell\ncurl -X POST -d "data" <URL>\n```\n\nReplace `"data"` with the actual data you want to send in the `POST` request, and `<URL>` with the URL you are making the request to.\n\nFor example, to send a `POST` request to `https://example.com/api` with JSON data `{"key": "value"}`, the command would look like:\n\n```shell\ncurl -X POST -d \'{"key": "value"}\' https://example.com/api\n```\n\nYou can also pass data from a file using the `@` prefix followed by the file path. For example, if the data is in a file named `data.json`, the command would be:\n\n```shell\ncurl -X POST -d @data.json https://example.com/api\n```\n\nThis will read the contents of `data.json` and send it as the body of the `POST` request.', role='assistant', function_call=None, tool_calls=None))], created=1708908513, model='ft:gpt-3.5-turbo-0613:personal::8wJ6CtY5', object='chat.completion', system_fingerprint=None, usage=CompletionUsage(completion_tokens=241, prompt_tokens=20, total_tokens=261))
>>> client.chat.completions.create(model=model, messages=[{"role": "user", "content": "Can you give me a recipe for jaffa-cake style biscuits?"}])
ChatCompletion(id='chatcmpl-8wJSwwKI6eGzVnQRI1QQPoNW0f0pJ', choices=[Choice(finish_reason='stop', index=0, logprobs=None, message=ChatCompletionMessage(content="Certainly! Here's a recipe for homemade Jaffa Cake-style biscuits:\n\nIngredients:\n- 200g self-raising flour\n- 50g cocoa powder\n- 150g unsalted butter (at room temperature)\n- 150g caster sugar\n- 2 large eggs (at room temperature)\n- 1 tsp vanilla extract\n- Zest of 1 orange (preferably organic)\n- 12 tsp orange marmalade\n- 150g dark chocolate (for coating)\n- 1-2 tsp vegetable oil (to thin the chocolate, if needed)\n\nInstructions:\n1. Preheat your oven to 180°C (350°F) and line a baking tray with parchment paper.\n2. In a bowl, whisk together the flour and cocoa powder, then set aside.\n3. In another large bowl, cream together the butter and caster sugar until pale and fluffy.\n4. Beat in the eggs, one at a time, ensuring each is fully incorporated. Add the vanilla extract and orange zest, mixing well.\n5. Gradually fold in the flour and cocoa mixture until a soft dough forms.\n6. Roll the dough into small balls, approximately 2-3cm in diameter.\n7. Place the dough balls onto the prepared baking tray, spacing them apart to allow for spreading. Flatten each ball slightly with the back of a spoon.\n8. Bake in the preheated oven for 10-12 minutes until the biscuits are firm to the touch. Be careful not to overbake.\n9. Remove the biscuits from the oven and allow them to cool on a wire rack.\n10. Once cooled, spread a teaspoon of orange marmalade onto the surface of each biscuit.\n11. In a heatproof bowl set over a pot of simmering water, melt the dark chocolate until smooth. If the chocolate is too thick, add a teaspoon or two of vegetable oil to thin it out.\n12. Using a fork or tongs, carefully dip each biscuit into the melted chocolate, ensuring they are fully coated. Place them back on the baking tray or a wire rack to set.\n13. Optional: Before the chocolate sets completely, you can lightly score the surface with a fork or a toothpick to mimic the traditional Jaffa Cake pattern.\n14. Allow the chocolate to set at room temperature or, for a faster finish, place the biscuits in the refrigerator for about 20-30 minutes.\n15. Once the chocolate is firm, your homemade Jaffa Cake-style biscuits are ready to enjoy!\n\nNote: This recipe yields approximately 24 biscuits, but you can easily adjust the quantities to make more or fewer as desired.", role='assistant', function_call=None, tool_calls=None))], created=1708908542, model='ft:gpt-3.5-turbo-0613:personal::8wJ6CtY5', object='chat.completion', system_fingerprint=None, usage=CompletionUsage(completion_tokens=539, prompt_tokens=22, total_tokens=561))
>>> 
KeyboardInterrupt
>>> 
```

## TTS Talk

I gave [that talk](https://guild.host/events/text-to-speech-ml-models-gdmhhw) I mentioned. It went really well, but there isn't a good record of it up anywhere. This is because [the video](https://static.inaimathi.ca/tts-talk.mp4) got misencoded :/ I've got a better strategy for the future, but this one is kind of beyond repair. Lets try extracting a transcript of it. 

- Try to transcribe the video, see about doing the meeting transcription thing. Possibly stream this process?
