#include <SPI.h>
#include <Wire.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>

#define SCREEN_WIDTH 128 
#define SCREEN_HEIGHT 64
#define OLED_RESET -1
#define ONE_SECOND_MS 1000

Adafruit_SSD1306 display(SCREEN_WIDTH, SCREEN_HEIGHT, &Wire, OLED_RESET);

const int pumpRelayPin = 2;         // Relay connected to pump (active low)
const int soilMoisturePin = A0;     // Capacitive soil moisture sensor
const int waterLevelPin = A1;       // Water level sensor 
const int flowMeterPin = 3;         // Flowmeter signal pin
const int joystickXPin = A3;        // Joystick X-axis potentiometer
const int joystickButtonPin = 8;   // Joystick button pin

// Thresholds (modifiable via modes)
int soilMoistureThreshold = 350;  // Adjust this value based on calibration
int waterLevelThreshold = 720;    // Adjust this value based on calibration
float waterVolumeTarget = 10.0;   // Target water volume in ml (for one watering cycle)

// Flowmeter variables
volatile int flowPulseCount = 0;
const float calibrationFactor = 5000; // Calibration factor (pulses per liter)

// Variables
float totalVolume = 0.0; // Total volume delivered (ml)
unsigned long lastWateringTime = 0; // Last time the soil moisture was checked
const unsigned long wateringInterval = 10 * ONE_SECOND_MS;
unsigned long sensorReadInterval = 5 * ONE_SECOND_MS; 
unsigned long lastSensorReadTime = 0; 
const int soilMoistureHysteresis = 50;
unsigned long lastResetTime = 0;
const unsigned long dailyResetInterval = 24 * 60 * 60 * ONE_SECOND_MS;

enum Mode { SET_SOIL_MOISTURE, SET_WATER_LEVEL, SET_WATER_VOLUME, CONFIRM_VALUES, RUN_PROGRAM };
Mode mode = SET_SOIL_MOISTURE;

bool lastJoystickButtonState = HIGH;
unsigned long lastDebounceTime = 0;
const unsigned long debounceDelay = 50;
unsigned long confirmStartTime = 0;
bool confirmTimerActive = false;

void flowPulseCounter() {
    flowPulseCount++;
}

const char* getSoilMoistureStatus(int soilMoistureValue) {
    return soilMoistureValue < soilMoistureThreshold ? "HAS WATER" : "NEEDS WATER";
}

const char* getWaterLevelStatus(int waterLevelValue) {
    if (waterLevelValue < 720) return "Out of water";
    if (waterLevelValue < 760) return "Water level low";
    return "NONE";
}

void updateDisplay(const char* soilMoistureStatus, const char* waterLevelStatus, bool waterAvailable, bool isIrrigating) {
    display.clearDisplay();
    display.setTextSize(1);
    display.setCursor(0, 0);
    display.println(F("=== Sensor Data ==="));
    display.print(F("Soil: "));
    display.println(soilMoistureStatus);
    display.print(F("Water Available: "));
    display.println(waterAvailable ? "Yes" : "No");
    // display.print(F("Pump Volume: "));
    // display.print(totalVolume);
    // display.println(F(" ml"));
    
    if (waterLevelStatus != "NONE" && !isIrrigating) {
        display.setTextSize(1);
        display.setCursor(0, 56);
        display.println(waterLevelStatus);
    }
    
    if (isIrrigating) {
        display.setTextSize(2);
        display.setCursor(0, 48);
        display.println(F("IRRIGATING"));
    }
    
    display.display();
}

void setup() {
    Serial.begin(9600);

    pinMode(pumpRelayPin, OUTPUT);
    pinMode(flowMeterPin, INPUT_PULLUP);
    pinMode(joystickButtonPin, INPUT_PULLUP);

    attachInterrupt(digitalPinToInterrupt(flowMeterPin), flowPulseCounter, RISING);

    digitalWrite(pumpRelayPin, HIGH);

    if (!display.begin(SSD1306_SWITCHCAPVCC, 0x3C)) {
        Serial.println(F("SSD1306 allocation failed"));
        for (;;);
    }
    display.clearDisplay();
    display.setTextSize(1);     
    display.setTextColor(WHITE);
    display.setCursor(0, 0);    
    display.println(F("System Initialized..."));
    display.println(F("Monitoring Sensors..."));
    display.display();

    Serial.println("System Initialized. Monitoring Sensors...");
    lastResetTime = millis();
}

void updateDisplayForMode() {
    display.clearDisplay();
    display.setTextSize(1);
    display.setCursor(0, 0);

    switch (mode) {
        case SET_SOIL_MOISTURE:
            display.println(F("=== Setup Mode ==="));
            display.println(F("Set Soil Moisture:"));
            display.print(F("Threshold: "));
            display.println(soilMoistureThreshold);
            break;

        case SET_WATER_LEVEL:
            display.println(F("=== Setup Mode ==="));
            display.println(F("Set Water Level:"));
            display.print(F("Threshold: "));
            display.println(waterLevelThreshold);
            break;

        case SET_WATER_VOLUME:
            display.println(F("=== Setup Mode ==="));
            display.println(F("Set Water Volume:"));
            display.print(F("Target: "));
            display.println(waterVolumeTarget);
            break;

        case CONFIRM_VALUES:
            display.println(F("Confirming Values..."));
            display.print(F("Soil: "));
            display.println(soilMoistureThreshold);
            display.print(F("Water: "));
            display.println(waterLevelThreshold);
            display.print(F("Volume: "));
            display.println(waterVolumeTarget);
            break;

        case RUN_PROGRAM:
            display.println(F("=== System Running ==="));
            display.println(F("Monitoring sensors..."));
            display.println(F("Press joystick to reconfigure."));
            break;

        default:
            break;
    }

    display.display();
}

void handleJoystick() {
    int joystickXValue = analogRead(joystickXPin);
    bool joystickButtonState = digitalRead(joystickButtonPin);
    if (joystickButtonState == LOW && lastJoystickButtonState == HIGH) {
        if (mode == RUN_PROGRAM) {
            mode = SET_SOIL_MOISTURE;
            Serial.println("Returning to setup mode...");
            updateDisplayForMode();
        } else if (mode == CONFIRM_VALUES) {
            confirmTimerActive = true;
            confirmStartTime = millis();
        } else {
            mode = static_cast<Mode>(mode + 1);
            Serial.print("Mode changed to: ");
            Serial.println(mode);
            updateDisplayForMode();
        }
    }

    if (joystickButtonState == HIGH) {
        if (mode == SET_SOIL_MOISTURE) {
            soilMoistureThreshold = map(joystickXValue, 0, 1023, 200, 800);
        } else if (mode == SET_WATER_LEVEL) {
            waterLevelThreshold = map(joystickXValue, 0, 1023, 700, 1000);
        } else if (mode == SET_WATER_VOLUME) {
            waterVolumeTarget = map(joystickXValue, 0, 1023, 1, 50);
        }
    }

    lastJoystickButtonState = joystickButtonState;
}


void loop() {
    unsigned long currentMillis = millis();

    if (mode == CONFIRM_VALUES && confirmTimerActive) {
        if (currentMillis - confirmStartTime >= 5000) {
            confirmTimerActive = false;
            mode = RUN_PROGRAM;
            Serial.println("System running...");
            updateDisplayForMode();
        } else {
            updateDisplayForMode();
        }
        return;
    }

    if (mode != RUN_PROGRAM) {
        handleJoystick();
        updateDisplayForMode();
        return;
    }

    if (currentMillis - lastSensorReadTime >= sensorReadInterval) {
        int soilMoistureValue = analogRead(soilMoisturePin);
        int waterLevelValue = analogRead(waterLevelPin);
        bool waterAvailable = (waterLevelValue > waterLevelThreshold);

        const char* soilMoistureStatus = getSoilMoistureStatus(soilMoistureValue);
        const char* waterLevelStatus = getWaterLevelStatus(waterLevelValue);

        updateDisplay(soilMoistureStatus, waterLevelStatus, waterAvailable, false);
        lastSensorReadTime = currentMillis;
    }

    if (currentMillis - lastWateringTime >= wateringInterval) {
        int soilMoistureValue = analogRead(soilMoisturePin);
        int waterLevelValue = analogRead(waterLevelPin);
        bool waterAvailable = (waterLevelValue > waterLevelThreshold);

        if (soilMoistureValue > soilMoistureThreshold && waterAvailable) {
            flowPulseCount = 0;
            totalVolume = 0.0;
            digitalWrite(pumpRelayPin, LOW);
            
            while (totalVolume < waterVolumeTarget) {
                totalVolume = (flowPulseCount / calibrationFactor) * 1000;
                updateDisplay(getSoilMoistureStatus(soilMoistureValue), getWaterLevelStatus(waterLevelValue), waterAvailable, true);
            }

            digitalWrite(pumpRelayPin, HIGH);
            lastWateringTime = millis();
        }

        updateDisplay(
            getSoilMoistureStatus(soilMoistureValue), 
            getWaterLevelStatus(waterLevelValue), 
            waterAvailable, 
            false
        );
    }

    if (currentMillis - lastResetTime >= dailyResetInterval) {
        totalVolume = 0.0;
        lastResetTime += dailyResetInterval; 
        Serial.println("Total water volume reset.");
    }
}