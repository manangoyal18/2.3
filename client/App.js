import React from 'react';
import { ImageBackground, StyleSheet } from 'react-native';
import { NavigationContainer } from '@react-navigation/native';
import { createNativeStackNavigator } from '@react-navigation/native-stack';

import HomeScreen from './src/screens/HomeScreen';
import SinglePlayerLevels from './src/screens/SinglePlayerLevels';
import Easy from './src/screens/Easy';
import Medium from './src/screens/Medium';
//import TwoPlayersLocal from './src/screens/TwoPlayersLocal';

import bg from './assets/bg.jpeg';
import TwoPlayersLocal from './src/screens/TwoPlayersLocal';
import TwoPlayersType from './src/screens/TwoPlayersType';
import OnlineGame from './src/screens/OnlineGame';

const Stack = createNativeStackNavigator();

export default function App() {
  return (
    <ImageBackground source={bg} style={styles.background} resizeMode="cover">
      <NavigationContainer>
        <Stack.Navigator initialRouteName="Home" screenOptions={{ headerShown: false }}>
          <Stack.Screen name="Home" component={HomeScreen} />
          <Stack.Screen name="SinglePlayerLevels" component={SinglePlayerLevels} />
          <Stack.Screen name="Easy" component={Easy} />
          <Stack.Screen name="Medium" component={Medium} />
          <Stack.Screen name="TwoPlayersType" component={TwoPlayersType} />
          <Stack.Screen name="TwoPlayersLocal" component={TwoPlayersLocal} />
           <Stack.Screen name="OnlineGame" component={OnlineGame} />
        </Stack.Navigator>
      </NavigationContainer>
    </ImageBackground>
  );
}

const styles = StyleSheet.create({
  background: {
    flex: 1,
    width: '100%',
    height: '100%',
  },
});
