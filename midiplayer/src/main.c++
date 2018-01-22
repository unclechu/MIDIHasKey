#include <stdlib.h>
#include <unistd.h>
#include <iostream>
#include <string>
#include <mutex>
#include <queue>
#include <vector>

#include <jack/jack.h>
#include <jack/midiport.h>

#define CLIENT_NAME "MIDIHasKey"
#define PORT_NAME   "output"

#define UNUSED(expr) (void)(expr); // to supress warnings

struct State_t
{
  jack_client_t *client;
  jack_port_t *output;
  std::mutex queue_mtx;
  std::queue<std::pair<jack_midi_data_t*, size_t>> events_queue;
};

int process(const jack_nframes_t nframes, void* const arg)
{
  State_t* state = (State_t*) arg;
  void *buf = jack_port_get_buffer(state->output, nframes);
  jack_midi_clear_buffer(buf);

  // do not wait for mutex to be released and do nothing in current buffer
  if (!state->queue_mtx.try_lock())
    return 0;

  // nothing to handle
  if (state->events_queue.empty()) {
    state->queue_mtx.unlock();
    return 0;
  }

  for (jack_nframes_t i=0; i<nframes; ++i) {
    if (state->events_queue.empty()) break;

    jack_midi_event_write(
      buf, i,
      state->events_queue.front().first,
      state->events_queue.front().second);

    delete state->events_queue.front().first;
    state->events_queue.pop();
  }

  state->queue_mtx.unlock();
  return 0;
}

State_t* init_jack()
{
  jack_status_t status;
  jack_client_t *client = jack_client_open(CLIENT_NAME, JackNullOption, &status, NULL);

  if (client == NULL) {
    std::cerr << "JACK: client isn't opened." << std::endl;
    exit(EXIT_FAILURE);
  }

  if (status & JackNameNotUnique) {
    std::cerr << "JACK: client name '" << CLIENT_NAME << "' is already taken." << std::endl;
    exit(EXIT_FAILURE);
  }

  jack_port_t *output =
    jack_port_register(client, PORT_NAME, JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0);

  if (output == NULL) {
    std::cerr << "JACK: registering '" << PORT_NAME << "' port failed." << std::endl;
    exit(EXIT_FAILURE);
  }

  State_t *state = new State_t();
  state->client = client;
  state->output = output;

  jack_set_process_callback(client, process, state);

  if (jack_activate(client)) {
    std::cerr << "JACK: activating client is failed." << std::endl;
    exit(EXIT_FAILURE);
  }

  return state;
}

inline void protect_empty_line(std::string line);

void read_pipe_loop(State_t *state)
{
  std::string single_cmd("single");
  std::string multiple_cmd("multiple");

  for (std::string line; std::getline(std::cin, line);) {

    if (line == single_cmd) {

      std::getline(std::cin, line); // read event size
      size_t ev_size = stoi(line);
      jack_midi_data_t *ev = new jack_midi_data_t[ev_size];

      for (size_t i=0; i<ev_size; ++i) {
        char byte;
        std::cin.read(&byte, sizeof(jack_midi_data_t));
        ev[i] = (unsigned char) byte;
      }

      {
        std::lock_guard<std::mutex> lock(state->queue_mtx);
        state->events_queue.push(std::pair<jack_midi_data_t*, size_t>(ev, ev_size));
      }

      std::getline(std::cin, line);
      protect_empty_line(line);

    } else if (line == multiple_cmd) {

      // Absorbing whole list of multiple events pack and only then putting it to the queue

      std::getline(std::cin, line); // read how many events in this multiple bundle
      std::vector<std::pair<jack_midi_data_t*, size_t>> events(stoi(line));

      for (size_t n=0, len=events.size(); n<len; ++n) {
        std::getline(std::cin, line); // read event size
        size_t ev_size = stoi(line);
        jack_midi_data_t *ev = new jack_midi_data_t[ev_size];

        for (size_t i=0; i<ev_size; ++i) {
          char byte;
          std::cin.read(&byte, sizeof(jack_midi_data_t));
          ev[i] = (jack_midi_data_t) byte;
        }

        events[n] = std::pair<jack_midi_data_t*, size_t>(ev, ev_size);
        std::getline(std::cin, line);
        protect_empty_line(line);
      }

      std::getline(std::cin, line);
      protect_empty_line(line);

      {
        std::lock_guard<std::mutex> lock(state->queue_mtx);
        for (auto it=events.begin(), e=events.end(); it!=e; ++it) state->events_queue.push(*it);
      }

    } else {
      std::cerr << "Unknown command: " << line << std::endl;
      exit(EXIT_FAILURE);
    }
  }
}

inline void protect_empty_line(std::string line)
{
  if (line.empty()) return;
  std::cerr << "This line supposed to be empty: '" << line << "'" << std::endl;
  exit(EXIT_FAILURE);
}

int main()
{
  read_pipe_loop(init_jack());
  return EXIT_SUCCESS;
}
