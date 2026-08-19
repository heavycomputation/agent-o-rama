(ns com.rpl.agent.customer-support
  "AI-powered customer support agent for airline booking assistance.

   Provides comprehensive travel support including flight search and booking,
   hotel reservations, car rentals, and policy information. Maintains customer
   context and supports complex multi-step interactions.

   Based on functionality in:

   https://github.com/langchain-ai/langgraph/blob/main/docs/docs/tutorials/customer-support/customer-support.ipynb"
  (:require
   [clojure.string :as str]
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as model]
   [com.rpl.agent-o-rama.model.openai :as openai]
   [com.rpl.agent-o-rama.schema :as schema]
   [com.rpl.agent-o-rama.store :as store]
   [com.rpl.agent-o-rama.tools :as tools]
   [com.rpl.rama :as rama]
   [com.rpl.rama.path :as path]
   [com.rpl.rama.test :as rtest]
   [jsonista.core :as j]
   [org.httpkit.client :as http])
  (:import
   [java.time
    LocalDateTime]
   [java.util
    UUID]))

(def CUSTOMER-SUPPORT-SYSTEM-MESSAGE
  "You are a helpful customer support assistant for Swiss Airlines.

   You help customers with comprehensive travel planning including flight
   bookings, changes, cancellations, and general travel assistance.

   You have access to tools to:
   - Search and view flight information
   - Update or cancel bookings
   - Search for hotels and make reservations
   - Find and book car rental options
   - Update or cancel car rental bookings
   - Search for local excursions and activities
   - Book excursions and experiences
   - Search the web for travel-related information
   - Look up company policies

   Always be polite, helpful, and professional. If you cannot help with
   something, explain clearly and suggest alternatives when possible.")

;; Mock databases - in a real system these would be external databases
(def MOCK-FLIGHTS
  [{:flight-id         "LX101"
    :departure-airport "ZUR"
    :arrival-airport   "JFK"
    :departure-time    "2024-03-15T08:00"
    :arrival-time      "2024-03-15T11:30"
    :price             850
    :available-seats   45}
   {:flight-id         "LX102"
    :departure-airport "JFK"
    :arrival-airport   "ZUR"
    :departure-time    "2024-03-15T14:30"
    :arrival-time      "2024-03-16T06:45"
    :price             920
    :available-seats   32}
   {:flight-id         "LX201"
    :departure-airport "ZUR"
    :arrival-airport   "LAX"
    :departure-time    "2024-03-16T10:15"
    :arrival-time      "2024-03-16T13:45"
    :price             1200
    :available-seats   28}])

(def MOCK-HOTELS
  [{:hotel-id        "H001"
    :name            "Grand Hotel"
    :location        "New York"
    :price-tier      "luxury"
    :price-per-night 350}
   {:hotel-id        "H002"
    :name            "City Inn"
    :location        "New York"
    :price-tier      "budget"
    :price-per-night 120}
   {:hotel-id        "H003"
    :name            "Business Lodge"
    :location        "Los Angeles"
    :price-tier      "business"
    :price-per-night 180}])

(def MOCK-CAR-RENTALS
  [{:rental-id     "R001"
    :location      "New York"
    :car-type      "Economy"
    :price-per-day 45
    :available     true}
   {:rental-id     "R002"
    :location      "New York"
    :car-type      "Luxury"
    :price-per-day 120
    :available     true}
   {:rental-id     "R003"
    :location      "Los Angeles"
    :car-type      "SUV"
    :price-per-day 85
    :available     true}])

(def MOCK-EXCURSIONS
  [{:excursion-id "E001"
    :name         "Statue of Liberty Tour"
    :location     "New York"
    :category     "sightseeing"
    :duration     "4 hours"
    :price        89
    :description
    "Visit the iconic Statue of Liberty and Ellis Island with guided tour"
    :available    true}
   {:excursion-id "E002"
    :name         "Central Park Walking Tour"
    :location     "New York"
    :category     "nature"
    :duration     "2 hours"
    :price        35
    :description  "Explore Central Park's highlights with an experienced guide"
    :available    true}
   {:excursion-id "E003"
    :name         "Broadway Show Package"
    :location     "New York"
    :category     "entertainment"
    :duration     "3 hours"
    :price        150
    :description  "Premium Broadway show tickets with pre-theater dinner"
    :available    true}
   {:excursion-id "E004"
    :name         "Hollywood Studio Tour"
    :location     "Los Angeles"
    :category     "entertainment"
    :duration     "6 hours"
    :price        125
    :description  "Behind-the-scenes tour of major Hollywood studios"
    :available    true}
   {:excursion-id "E005"
    :name         "Santa Monica Beach Experience"
    :location     "Los Angeles"
    :category     "nature"
    :duration     "3 hours"
    :price        45
    :description  "Beach activities, pier visit, and sunset viewing"
    :available    true}])

(def POLICIES
  {"baggage"
   "Carry-on: 1 bag up to 8kg. Checked baggage: 1 bag up to 23kg included in economy."
   "cancellation"
   "Free cancellation up to 24 hours before departure. Cancellation fee applies after."
   "change-fee"
   "Flight changes: 50 EUR fee for economy, free for business class."
   "refund"
   "Refunds processed within 7-14 business days to original payment method."})

;; Tool functions
(defn fetch-user-flight-information
  "Retrieve flight information for a specific passenger."
  [agent-node {:keys [passenger-id]} arguments]
  (let [bookings-store (aor/get-store agent-node "$$bookings")
        passenger-id   (get arguments "passenger-id" passenger-id)]
    (if-let [booking (store/get bookings-store passenger-id)]
      (j/write-value-as-string
       {:status  "success"
        :booking booking
        :message
        (format
         "Found booking for passenger %s: Flight %s from %s to %s on %s"
         passenger-id
         (:flight-id booking)
         (:departure-airport booking)
         (:arrival-airport booking)
         (:departure-date booking))})
      (j/write-value-as-string
       {:status  "not-found"
        :message (format
                  "No booking found for passenger ID: %s"
                  passenger-id)}))))

(defn search-flights
  "Search for available flights between airports."
  [agent-node config arguments]
  (let [departure-airport (get arguments "departure-airport")
        arrival-airport   (get arguments "arrival-airport")
        start-date        (get arguments "start-date")
        end-date          (get arguments "end-date")
        flights-by-departure-store (aor/get-store agent-node
                                                  "$$flights-by-departure")
        departure-flights (store/get flights-by-departure-store
                                     departure-airport)
        matching-flights  (filterv
                           (fn [flight]
                             (and
                              (= (:arrival-airport flight) arrival-airport)
                              (>= (:available-seats flight) 1)))
                           (or departure-flights []))]
    (j/write-value-as-string
     {:status  "success"
      :flights matching-flights
      :message (format
                "Found %d flights from %s to %s"
                (count matching-flights)
                departure-airport
                arrival-airport)})))

(defn update-ticket-to-new-flight
  "Update a passenger's ticket to a new flight."
  [agent-node config arguments]
  (let [ticket-no           (get arguments "ticket-no")
        new-flight-id       (get arguments "new-flight-id")
        bookings-store      (aor/get-store agent-node "$$bookings")
        flights-by-id-store (aor/get-store agent-node "$$flights-by-id")
        flight              (store/get flights-by-id-store new-flight-id)]
    (if flight
      (do
        (store/put! bookings-store
                    ticket-no
                    (merge flight
                           {:ticket-no    ticket-no
                            :booking-date (str (LocalDateTime/now))}))
        (j/write-value-as-string
         {:status  "success"
          :message (format
                    "Successfully updated ticket %s to flight %s"
                    ticket-no
                    new-flight-id)}))
      (j/write-value-as-string
       {:status  "error"
        :message
        (format "Flight %s not found or not available" new-flight-id)}))))

(defn cancel-ticket
  "Cancel a specific ticket."
  [agent-node config arguments]
  (let [ticket-no      (get arguments "ticket-no")
        bookings-store (aor/get-store agent-node "$$bookings")]
    (if (store/get bookings-store ticket-no)
      (do
        (store/pstate-transform!
         [(path/keypath ticket-no) path/NONE]
         bookings-store
         ticket-no)
        (j/write-value-as-string
         {:status  "success"
          :message (format
                    "Ticket %s has been cancelled successfully"
                    ticket-no)}))
      (j/write-value-as-string
       {:status  "not-found"
        :message (format "Ticket %s not found" ticket-no)}))))

(defn search-hotels
  "Search for hotels in a location."
  [agent-node config arguments]
  (let [location        (get arguments "location")
        name            (get arguments "name")
        price-tier      (get arguments "price-tier")
        checkin-date    (get arguments "checkin-date")
        checkout-date   (get arguments "checkout-date")
        hotels-by-location-store (aor/get-store agent-node
                                                "$$hotels-by-location")
        location-hotels (store/get hotels-by-location-store location)
        matching-hotels (filter
                         (fn [hotel]
                           (and (or (nil? price-tier)
                                    (= (:price-tier hotel) price-tier))
                                (or (nil? name)
                                    (str/includes?
                                     (str/lower-case (:name hotel))
                                     (str/lower-case name)))))
                         (or location-hotels []))]
    (j/write-value-as-string
     {:status  "success"
      :hotels  matching-hotels
      :message (format
                "Found %d hotels in %s"
                (count matching-hotels)
                location)})))

(defn book-hotel
  "Book a hotel reservation."
  [agent-node {:keys [passenger-id]} arguments]
  (let [hotel-id           (get arguments "hotel-id")
        checkin-date       (get arguments "checkin-date")
        checkout-date      (get arguments "checkout-date")
        hotel-bookings-store (aor/get-store agent-node "$$hotel-bookings")
        hotels-by-id-store (aor/get-store agent-node "$$hotels-by-id")
        hotel              (store/get hotels-by-id-store hotel-id)
        booking-id         (str (UUID/randomUUID))]
    (if hotel
      (do
        (store/put! hotel-bookings-store
                    booking-id
                    (merge hotel
                           {:booking-id    booking-id
                            :passenger-id  passenger-id
                            :checkin-date  checkin-date
                            :checkout-date checkout-date
                            :booking-date  (str (LocalDateTime/now))}))
        (j/write-value-as-string
         {:status     "success"
          :booking-id booking-id
          :message    (format "Successfully booked %s for %s to %s"
                              (:name hotel)
                              checkin-date
                              checkout-date)}))
      (j/write-value-as-string
       {:status  "error"
        :message (format "Hotel %s not found" hotel-id)}))))

(defn search-car-rentals
  "Search for car rental options."
  [agent-node config arguments]
  (let [location         (get arguments "location")
        start-date       (get arguments "start-date")
        end-date         (get arguments "end-date")
        car-type         (get arguments "car-type")
        car-rentals-by-location-store
        (aor/get-store agent-node "$$car-rentals-by-location")
        location-rentals (store/get car-rentals-by-location-store location)
        matching-rentals (filter (fn [rental]
                                   (and (:available rental)
                                        (or (nil? car-type)
                                            (= (:car-type rental) car-type))))
                          (or location-rentals []))]
    (j/write-value-as-string
     {:status  "success"
      :rentals matching-rentals
      :message (format
                "Found %d car rentals in %s"
                (count matching-rentals)
                location)})))

(defn book-car-rental
  "Book a car rental."
  [agent-node {:keys [passenger-id]} arguments]
  (let [rental-id          (get arguments "rental-id")
        start-date         (get arguments "start-date")
        end-date           (get arguments "end-date")
        car-bookings-store (aor/get-store agent-node "$$car-bookings")
        car-rentals-by-id-store (aor/get-store agent-node "$$car-rentals-by-id")
        rental             (store/get car-rentals-by-id-store rental-id)
        booking-id         (str (UUID/randomUUID))]
    (if (and rental (:available rental))
      (do
        (store/put! car-bookings-store
                    booking-id
                    (merge rental
                           {:booking-id   booking-id
                            :passenger-id passenger-id
                            :start-date   start-date
                            :end-date     end-date
                            :booking-date (str (LocalDateTime/now))}))
        (j/write-value-as-string
         {:status     "success"
          :booking-id booking-id
          :message    (format "Successfully booked %s car for %s to %s"
                              (:car-type rental)
                              start-date
                              end-date)}))
      (j/write-value-as-string
       {:status  "error"
        :message (format "Car rental %s not available" rental-id)}))))

(defn search-excursions
  "Search for available excursions in a location."
  [agent-node config arguments]
  (let [location (get arguments "location")
        category (get arguments "category")
        excursions-by-location-store
        (aor/get-store agent-node "$$excursions-by-location")
        location-excursions (store/get excursions-by-location-store location)
        matching-excursions (filter (fn [excursion]
                                      (and (:available excursion)
                                           (or (nil? category)
                                               (= (:category excursion)
                                                  category))))
                             (or location-excursions []))]
    (j/write-value-as-string
     {:status     "success"
      :excursions matching-excursions
      :message    (format
                   "Found %d excursions in %s"
                   (count matching-excursions)
                   location)})))

(defn book-excursion
  "Book an excursion."
  [agent-node {:keys [passenger-id]} arguments]
  (let [excursion-id (get arguments "excursion-id")
        date         (get arguments "date")
        excursion-bookings-store (aor/get-store agent-node
                                                "$$excursion-bookings")
        excursions-by-id-store (aor/get-store agent-node "$$excursions-by-id")
        excursion    (store/get excursions-by-id-store excursion-id)
        booking-id   (str (UUID/randomUUID))]
    (if (and excursion (:available excursion))
      (do
        (store/put! excursion-bookings-store
                    booking-id
                    (merge excursion
                           {:booking-id   booking-id
                            :passenger-id passenger-id
                            :date         date
                            :booking-date (str (LocalDateTime/now))}))
        (j/write-value-as-string
         {:status     "success"
          :booking-id booking-id
          :message    (format "Successfully booked %s for %s"
                              (:name excursion)
                              date)}))
      (j/write-value-as-string
       {:status  "error"
        :message (format "Excursion %s not available" excursion-id)}))))

(defn update-car-rental
  "Update an existing car rental booking."
  [agent-node config arguments]
  (let [booking-id         (get arguments "booking-id")
        start-date         (get arguments "start-date")
        end-date           (get arguments "end-date")
        car-bookings-store (aor/get-store agent-node "$$car-bookings")]
    (if-let [existing-booking (store/get car-bookings-store booking-id)]
      (do
        (store/put! car-bookings-store
                    booking-id
                    (merge existing-booking
                           {:start-date   start-date
                            :end-date     end-date
                            :last-updated (str (LocalDateTime/now))}))
        (j/write-value-as-string
         {:status  "success"
          :message (format "Successfully updated car rental booking %s"
                           booking-id)}))
      (j/write-value-as-string
       {:status  "not-found"
        :message (format "Car rental booking %s not found" booking-id)}))))

(defn cancel-car-rental
  "Cancel a car rental booking."
  [agent-node config arguments]
  (let [booking-id         (get arguments "booking-id")
        car-bookings-store (aor/get-store agent-node "$$car-bookings")]
    (if (store/get car-bookings-store booking-id)
      (do
        (store/pstate-transform!
         [(path/keypath booking-id) path/NONE]
         car-bookings-store
         booking-id)
        (j/write-value-as-string
         {:status  "success"
          :message (format
                    "Car rental booking %s has been cancelled successfully"
                    booking-id)}))
      (j/write-value-as-string
       {:status  "not-found"
        :message (format "Car rental booking %s not found" booking-id)}))))

(defn- tavily-search
  "Performs a Tavily web search, returning a vector of result maps with
  \"title\", \"url\", and \"content\" keys."
  [api-key query max-results]
  (let [{:keys [status body]}
        @(http/post
          "https://api.tavily.com/search"
          {:headers {"Content-Type" "application/json"}
           :body    (j/write-value-as-string
                     {"api_key"         api-key
                      "query"           query
                      "max_results"     max-results
                      "exclude_domains" ["en.wikipedia.org"]})
           :timeout 60000})]
    (if (= status 200)
      (vec (get (j/read-value body) "results"))
      (throw (ex-info "Tavily search failed" {:status status :body body})))))

(defn web-search
  "Perform web search for travel-related information using Tavily."
  [agent-node config arguments]
  (let [query     (get arguments "query")
        api-key   (aor/get-agent-object agent-node "tavily-api-key")
        results   (tavily-search api-key query 5)
        documents (mapv (fn [result]
                          {:title   (get result "title")
                           :url     (get result "url")
                           :snippet (get result "content")})
                        results)]
    (j/write-value-as-string
     {:status  "success"
      :results documents
      :message (format "Found %d search results for '%s'"
                       (count documents)
                       query)})))

(defn lookup-policy
  "Look up company policy information."
  [agent-node config arguments]
  (let [query             (get arguments "query")
        query-lower       (str/lower-case query)
        policies-store    (aor/get-store agent-node "$$policies")
        all-policies      (store/get policies-store "all-policies")
        matching-policies (filter (fn [[key _]]
                                    (str/includes? query-lower key))
                           all-policies)]
    (if (seq matching-policies)
      (j/write-value-as-string
       {:status   "success"
        :policies (into {} matching-policies)
        :message  (format "Found %d policy matches for '%s'"
                          (count matching-policies)
                          query)})
      (j/write-value-as-string
       {:status  "not-found"
        :message (format "No policies found matching '%s'" query)}))))

;; Tool definitions for new functionality

;; Tool definitions using agent-o-rama tools framework
(def CUSTOMER-SUPPORT-TOOLS
  [(tools/tool
    {:name        "fetch_user_flight_information"
     :description
     "Retrieve current flight booking information for a specific passenger"
     :schema      (schema/object
                   {:required ["passenger-id"]}
                   {"passenger-id" (schema/string "The passenger ID to look up")})}
    fetch-user-flight-information
    {:include-context? true})

   (tools/tool
    {:name        "search_flights"
     :description "Search for available flights between airports"
     :schema      (schema/object
                   {:required ["departure-airport" "arrival-airport"]}
                   {"departure-airport" (schema/string "3-letter departure airport code")
                    "arrival-airport"   (schema/string "3-letter arrival airport code")
                    "start-date"        (schema/string "Earliest departure date (YYYY-MM-DD)")
                    "end-date"          (schema/string "Latest departure date (YYYY-MM-DD)")})}
    search-flights
    {:include-context? true})

   (tools/tool
    {:name        "update_ticket_to_new_flight"
     :description "Update an existing ticket to a new flight"
     :schema      (schema/object
                   {:required ["ticket-no" "new-flight-id"]}
                   {"ticket-no"     (schema/string "Ticket number to update")
                    "new-flight-id" (schema/string "New flight ID to change to")})}
    update-ticket-to-new-flight
    {:include-context? true})

   (tools/tool
    {:name        "cancel_ticket"
     :description "Cancel a flight ticket"
     :schema      (schema/object
                   {:required ["ticket-no"]}
                   {"ticket-no" (schema/string "Ticket number to cancel")})}
    cancel-ticket
    {:include-context? true})

   (tools/tool
    {:name        "search_hotels"
     :description "Search for hotels in a specific location"
     :schema      (schema/object
                   {:required ["location"]}
                   {"location"      (schema/string "City or location to search")
                    "name"          (schema/string "Hotel name to search for")
                    "price-tier"    (schema/enum
                                     "Price category preference"
                                     ["budget" "business" "luxury"])
                    "checkin-date"  (schema/string "Check-in date (YYYY-MM-DD)")
                    "checkout-date" (schema/string "Check-out date (YYYY-MM-DD)")})}
    search-hotels
    {:include-context? true})

   (tools/tool
    {:name        "book_hotel"
     :description "Book a hotel reservation"
     :schema      (schema/object
                   {:required ["hotel-id" "checkin-date" "checkout-date"]}
                   {"hotel-id"      (schema/string "Hotel ID to book")
                    "checkin-date"  (schema/string "Check-in date (YYYY-MM-DD)")
                    "checkout-date" (schema/string "Check-out date (YYYY-MM-DD)")})}
    book-hotel
    {:include-context? true})

   (tools/tool
    {:name        "search_car_rentals"
     :description "Search for car rental options"
     :schema      (schema/object
                   {:required ["location"]}
                   {"location"   (schema/string "City or location for car rental")
                    "start-date" (schema/string "Rental start date (YYYY-MM-DD)")
                    "end-date"   (schema/string "Rental end date (YYYY-MM-DD)")
                    "car-type"   (schema/string "Preferred car type")})}
    search-car-rentals
    {:include-context? true})

   (tools/tool
    {:name        "book_car_rental"
     :description "Book a car rental"
     :schema      (schema/object
                   {:required ["rental-id" "start-date" "end-date"]}
                   {"rental-id"  (schema/string "Car rental ID to book")
                    "start-date" (schema/string "Rental start date (YYYY-MM-DD)")
                    "end-date"   (schema/string "Rental end date (YYYY-MM-DD)")})}
    book-car-rental
    {:include-context? true})

   (tools/tool
    {:name        "lookup_policy"
     :description "Look up company policies and procedures"
     :schema      (schema/object
                   {:required ["query"]}
                   {"query"
                    (schema/string
                     "Policy topic to search for (e.g., baggage, cancellation, refund)")})}
    lookup-policy
    {:include-context? true})

   (tools/tool
    {:name        "search_excursions"
     :description "Search for available excursions and activities"
     :schema      (schema/object
                   {:required ["location"]}
                   {"location" (schema/string "City or location to search for excursions")
                    "category" (schema/enum
                                "Type of excursion"
                                ["sightseeing" "nature" "entertainment" "adventure"])})}
    search-excursions
    {:include-context? true})

   (tools/tool
    {:name        "book_excursion"
     :description "Book an excursion or activity"
     :schema      (schema/object
                   {:required ["excursion-id" "date"]}
                   {"excursion-id" (schema/string "Excursion ID to book")
                    "date"         (schema/string "Date for the excursion (YYYY-MM-DD)")})}
    book-excursion
    {:include-context? true})

   (tools/tool
    {:name        "update_car_rental"
     :description "Update an existing car rental booking"
     :schema      (schema/object
                   {:required ["booking-id" "start-date" "end-date"]}
                   {"booking-id" (schema/string "Car rental booking ID to update")
                    "start-date" (schema/string "New rental start date (YYYY-MM-DD)")
                    "end-date"   (schema/string "New rental end date (YYYY-MM-DD)")})}
    update-car-rental
    {:include-context? true})

   (tools/tool
    {:name        "cancel_car_rental"
     :description "Cancel a car rental booking"
     :schema      (schema/object
                   {:required ["booking-id"]}
                   {"booking-id" (schema/string "Car rental booking ID to cancel")})}
    cancel-car-rental
    {:include-context? true})

   (tools/tool
    {:name        "web_search"
     :description "Search for travel-related information online"
     :schema      (schema/object
                   {:required ["query"]}
                   {"query" (schema/string "Search query for travel information")})}
    web-search
    {:include-context? true})])

;; Initialization will be handled by the initialization agent

(aor/defagentmodule CustomerSupportModule
  [topology]

  ;; Declare OpenAI model
  (openai/declare-model
   topology
   "openai-model"
   {:api-key-env "OPENAI_API_KEY"
    :model       "gpt-5-mini"})

  (aor/declare-agent-object
   topology
   "tavily-api-key"
   (System/getenv "TAVILY_API_KEY"))

  ;; Declare stores for persistent data
  (aor/declare-key-value-store topology "$$bookings" String Object)
  (aor/declare-key-value-store topology "$$hotel-bookings" String Object)
  (aor/declare-key-value-store topology "$$car-bookings" String Object)
  (aor/declare-key-value-store topology "$$conversations" String Object)

  ;; Declare stores for reference data

  ;; departure airport -> list of flights
  (aor/declare-key-value-store topology "$$flights-by-departure" String Object)
  ;; flight-id -> flight details
  (aor/declare-key-value-store topology "$$flights-by-id" String Object)
  ;; location -> list of hotels
  (aor/declare-key-value-store topology "$$hotels-by-location" String Object)
  ;; hotel-id -> hotel details
  (aor/declare-key-value-store topology "$$hotels-by-id" String Object)
  ;; location -> list of car rentals
  (aor/declare-key-value-store topology
                               "$$car-rentals-by-location"
                               String
                               Object)
  ;; rental-id -> rental details

  ;; location -> list of excursions
  ;; rental-id -> rental details
  (aor/declare-key-value-store topology "$$car-rentals-by-id" String Object)

  ;; excursion-bookings store
  (aor/declare-key-value-store topology "$$excursion-bookings" String Object)
  ;; location -> list of excursions
  (aor/declare-key-value-store topology
                               "$$excursions-by-location"
                               String
                               Object)
  ;; excursion-id -> excursion details
  (aor/declare-key-value-store topology "$$excursions-by-id" String Object)

  (aor/declare-key-value-store topology "$$policies" String Object)

  ;; Define the initialization agent
  (->
    topology
    (aor/new-agent "data-initializer")
    (aor/node
     "initialize"
     nil
     (fn iniitalize-node [agent-node]
       (let [flights-by-departure-store (aor/get-store agent-node
                                                       "$$flights-by-departure")
             flights-by-id-store        (aor/get-store agent-node
                                                       "$$flights-by-id")
             hotels-by-location-store   (aor/get-store agent-node
                                                       "$$hotels-by-location")
             hotels-by-id-store         (aor/get-store agent-node
                                                       "$$hotels-by-id")
             car-rentals-by-location-store
             (aor/get-store agent-node "$$car-rentals-by-location")
             car-rentals-by-id-store    (aor/get-store agent-node
                                                       "$$car-rentals-by-id")
             excursion-bookings-store   (aor/get-store agent-node
                                                       "$$excursion-bookings")
             excursions-by-location-store (aor/get-store
                                           agent-node
                                           "$$excursions-by-location")
             excursions-by-id-store     (aor/get-store agent-node
                                                       "$$excursions-by-id")
             policies-store             (aor/get-store agent-node "$$policies")
             bookings-store             (aor/get-store agent-node "$$bookings")]

         ;; Populate flights stores - organize by departure airport and by ID
         (let [flights-by-departure (group-by :departure-airport MOCK-FLIGHTS)]
           (doseq [[departure-airport flights] flights-by-departure]
             (store/put! flights-by-departure-store departure-airport flights)))

         (doseq [flight MOCK-FLIGHTS]
           (store/put! flights-by-id-store (:flight-id flight) flight))

         ;; Populate hotels stores - organize by location and by ID
         (let [hotels-by-location (group-by :location MOCK-HOTELS)]
           (doseq [[location hotels] hotels-by-location]
             (store/put! hotels-by-location-store location hotels)))

         (doseq [hotel MOCK-HOTELS]
           (store/put! hotels-by-id-store (:hotel-id hotel) hotel))

         ;; Populate car rentals stores - organize by location and by ID
         (let [car-rentals-by-location (group-by :location MOCK-CAR-RENTALS)]
           (doseq [[location car-rentals] car-rentals-by-location]
             (store/put! car-rentals-by-location-store location car-rentals)))

         (doseq [rental MOCK-CAR-RENTALS]
           (store/put! car-rentals-by-id-store (:rental-id rental) rental))

         ;; Populate excursions stores - organize by location and by ID
         (let [excursions-by-location (group-by :location MOCK-EXCURSIONS)]
           (doseq [[location excursions] excursions-by-location]
             (store/put! excursions-by-location-store location excursions)))

         (doseq [excursion MOCK-EXCURSIONS]
           (store/put! excursions-by-id-store
                       (:excursion-id excursion)
                       excursion))

         ;; Populate policies store - store all policies as a single map
         (store/put! policies-store "all-policies" POLICIES)

         ;; Create sample tickets for testing
         (store/put! bookings-store
                     "T789"
                     {:ticket-no    "T789"
                      :passenger-id "TEST126"
                      :flight-id    "LX101"
                      :status       "confirmed"
                      :booking-date "2024-01-15T10:30:00"})

         (store/put! bookings-store
                     "T456"
                     {:ticket-no    "T456"
                      :passenger-id "TEST125"
                      :flight-id    "LX101"
                      :status       "confirmed"
                      :booking-date "2024-01-10T14:20:00"})

         (aor/result! agent-node
                      "Reference data stores initialized successfully")))))

  ;; Define the customer support agent workflow
  (->
    topology
    (aor/new-agent "customer-support")

    ;; Main assistant node - handles conversation and tool decisions
    (aor/node
     "chat"
     "chat"
     (fn [agent-node messages config]
       (let [openai             (aor/get-agent-object
                                 agent-node
                                 "openai-model")
             conversation-store (aor/get-store agent-node "$$conversations")
             {:keys [passenger-id]} config
             tools              (aor/agent-client agent-node "tools")

             ;; Build conversation history
             all-messages       (into [(model/system
                                        CUSTOMER-SUPPORT-SYSTEM-MESSAGE)]
                                      messages)

             ;; Make API call with tools
             {:keys [message tool-calls text]}
             (model/chat openai
                         {:messages all-messages
                          :tools    CUSTOMER-SUPPORT-TOOLS})]

         ;; Store conversation state
         (when passenger-id
           (store/put! conversation-store
                       passenger-id
                       {:messages     (conj messages message)
                        :last-updated (str (LocalDateTime/now))}))

         ;; Check if assistant wants to use tools
         (if (seq tool-calls)
           (let [tool-results  (aor/agent-invoke tools tool-calls config)
                 next-messages (into (conj messages message) tool-results)]
             (aor/emit! agent-node "chat" next-messages config))
           (aor/result! agent-node text))))))

  (tools/new-tools-agent topology "tools" CUSTOMER-SUPPORT-TOOLS))

;;; Example invocation

(defn run-agent
  "Start the customer support agent with sample interactions."
  []
  (println "Starting Customer Support Agent...")
  (with-open [ipc (rtest/create-ipc)
              _ (aor/start-ui ipc)]
    ;; Launch the topology
    (rtest/launch-module! ipc CustomerSupportModule {:tasks 4 :threads 2})

    (let [module-name   (rama/get-module-name CustomerSupportModule)
          agent-manager (aor/agent-manager ipc module-name)
          initializer   (aor/agent-client agent-manager "data-initializer")
          agent         (aor/agent-client agent-manager "customer-support")]

      ;; Initialize stores with mock data using the initialization agent
      (println "Initializing reference data stores...")
      (let [init-result (aor/agent-invoke initializer)]
        (println init-result))

      ;; Sample interactions
      (println "\n=== Sample Customer Support Interactions ===\n")

      ;; Test 1: Flight search
      (println "🔍 Testing flight search...")
      (let [result (aor/agent-invoke
                    agent
                    [(model/user
                      "I need to find flights from ZUR to JFK for March 15th")]
                    {:passenger-id "P123"})]
        (println "Customer:"
                 "I need to find flights from ZUR to JFK for March 15th")
        (println "Agent:" result)
        (println))

      ;; Test 2: Policy lookup
      (println "📋 Testing policy lookup...")
      (let [result (aor/agent-invoke
                    agent
                    [(model/user "What is your baggage policy?")]
                    {:passenger-id "P124"})]
        (println "Customer:" "What is your baggage policy?")
        (println "Agent:" result)
        (println))

      ;; Test 3: Hotel search
      (println "🏨 Testing hotel search...")
      (let
        [result
         (aor/agent-invoke
          agent
          [(model/user
            "I need a hotel in New York for March 15-17, preferably budget-friendly")]
          {:passenger-id "P125"})]
        (println
         "Customer:"
         "I need a hotel in New York for March 15-17, preferably budget-friendly")
        (println "Agent:" result)
        (println))

      (println "Customer Support Agent completed sample interactions!")
      agent)))
